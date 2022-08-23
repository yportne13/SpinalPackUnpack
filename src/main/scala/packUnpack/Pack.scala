package packUnpack

import spinal.core._
import spinal.lib._

object Pack {
  def apply(list: PList[PackTrait])(start: Bool): Stream[Bits] = new Area {
    this.setName("")

    val ldist = list.distinct()

    val state = Reg(UInt(log2Up(list.length) bits)) init(0)
    val cnt = Reg(UInt(ldist.map(l => l.long.getWidth).max bits)) init(0)
    val stateCnt = (0 until list.depth).map(idx => Reg(UInt(10 bits)) init(0)).toList//TODO:width

    ldist.foreach{case l =>
      l.onMatch :=
        list.zipWithIdx.filter(_._1 == l).map(state === _._2).reduce(_ || _)
    }

    val cntIncrement = ldist
      .map(l => l.onActive)
      .reduce(_ || _)
    val cntEnd = ldist
      .map(l => (l.onMatch,l.long-1))
      .reduce((a,b) => (False, b._1?b._2|a._2))._2
    cnt := cntIncrement?((cnt===cntEnd)?U(0)|cnt+1)|cnt

    def stateJump(list: PList[(PackTrait, Int)])(stateNext: UInt, stateCnt: List[UInt]): PList[UInt] = {
      list match {
        case ACons(head, Nil) => ACons(stateNext, Nil)
        case MCons(head, repeat, Nil) => {
          MCons(
            stateJump(head)((stateCnt(0)===(repeat-1))?stateNext|U(head.first._2),stateCnt.drop(1)),
            repeat,
            Nil
          )
        }
        case ACons(head, tail) => {
          ACons(state+1, stateJump(tail)(stateNext, stateCnt))
        }
        case MCons(head, repeat, tail) => {
          MCons(
            stateJump(head)((stateCnt(0)===(repeat-1))?(state+1)|U(head.first._2), stateCnt.drop(1)),
            repeat,
            stateJump(tail)(stateNext, stateCnt)
          )
        }
        case Nil => Nil
      }
    }
    val stateNextList = stateJump(list.zipWithIdx)(U(0), stateCnt)
    val stateIncrement = (state === 0 && start) || (cntIncrement && (cnt === cntEnd))
    when(stateIncrement) {
      switch(state) {
        for(i <- 0 until list.length) {
          is(i) {
            state := stateNextList(i)
          }
        }
      }
    }

    def stateCntJump(list: PList[Int])(stateCntDo: List[(UInt => UInt, Int)]): PList[List[(UInt => UInt, Int)]] = {
      val cntnext: UInt => Int => (UInt => UInt, Int) = r => dep =>
        (x => (x===r-1)?U(0)|(x+1), dep)
      list match {
        case ACons(head, Nil) => ACons(stateCntDo, Nil)
        case MCons(head, repeat, Nil) => {
          MCons(
            stateCntJump(head)(stateCntDo :+ cntnext(repeat)(head.first-1)),
            repeat,
            Nil
          )
        }
        case ACons(head, tail) => {
          ACons(List(), stateCntJump(tail)(stateCntDo))
        }
        case MCons(head, repeat, tail) => {
          MCons(
            stateCntJump(head)(List(cntnext(repeat)(head.first-1))),
            repeat,
            stateCntJump(tail)(stateCntDo)
          )
        }
        case Nil => Nil
      }
    }

    val stateCntDo = stateCntJump(list.zipWithDepth.map(_._2))(List())
    when(stateIncrement) {
      switch(state) {
        for(i <- 0 until list.length) {
          is(i) {
            stateCntDo(i).foreach{case (job, depth) =>
              stateCnt(depth) := job(stateCnt(depth))
            }
          }
        }
      }
    }

    val ret = Stream(Bits(8 bits))//TODO:bitwidth
    ldist.foreach(l => l.output.ready := ret.ready)

    switch(state) {
      for(i <- 0 until list.length) {
        is(i) {
          ret.valid := list(i).output.valid
          ret.payload := list(i).output.payload
        }
      }
    }

  }.ret
}
