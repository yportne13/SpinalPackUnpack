package packUnpack

import spinal.core._
import spinal.lib._

sealed trait PList[+T] {

  def +[A](that: A): PList[T] = {
    this match {
      case Nil => {
        that match {
          case t: PList[T] => t
          case t: T => ACons(t, Nil)//TODO:
        }
      }
      case ACons(h, t) => ACons(h, t+that)
      case MCons(h, r, t) => MCons(h, r, t+that)
    }
  }

  def *(repeat: UInt): PList[T] = MCons(this, repeat, Nil)

  def *(repeat: Int): PList[T] = this*U(repeat)

  def first: T = this match {
    case ACons(head, tail) => head
    case MCons(head, repeat, tail) => head.first
    case Nil => null.asInstanceOf[T]//TODO:panic
  }

  def apply(idx: Int): T = this.zipWithIdx.filter(_._2 == idx).map(_._1).head

  def fold[A](acc: A)(f: (A,T) => A): A = this match {
    case Nil => acc
    case ACons(head, tail) => tail.fold(f(acc,head))(f)
    case MCons(head, repeat, tail) => tail.fold(head.fold(acc)(f))(f)
  }
  
  def map[A](f: T => A): PList[A] = {
    this match {
      case Nil => Nil
      case ACons(head, tail) => ACons(f(head), tail.map(f))
      case MCons(head, repeat, tail) => MCons(head.map(f), repeat, tail.map(f))
    }
  }

  def zipWithIdx: PList[(T, Int)] = {
    var idx = 0
    this.map{case x =>
      val ret = (x,idx)
      idx = idx + 1//TODO:remove side effect
      ret
    }
  }

  def zipWithDepth: PList[(T, Int)] = {
    def depthBuilder(t: PList[T])(depth: Int): PList[(T, Int)] = {
      t match {
        case Nil => Nil
        case ACons(head, tail) => ACons((head, depth), depthBuilder(tail)(depth))
        case MCons(head, repeat, tail) => MCons(depthBuilder(head)(depth+1), repeat, depthBuilder(tail)(depth))
      }
    }
    depthBuilder(this)(0)
  }

  def reduce[U >: T](f: (U,T) => U): U = this match {
    case ACons(head: U, tail) => tail.fold(head)(f)
    case MCons(head, repeat, tail) => tail.fold(head.reduce(f))(f)
    case Nil => null.asInstanceOf[T]//TODO
  }

  def distinct(): List[T] = {
    this.fold(List[T]()){case (l,i) => if(l.contains(i)) l else l :+ i}
  }

  def filter(f: T => Boolean): List[T] = {
    this.fold(List[T]()){case (l,i) => if(f(i)) l :+ i else l}
  }

  def length: Int = {
    this match {
      case Nil => 0
      case ACons(head, tail) => 1 + tail.length
      case MCons(head, repeat, tail) => head.length + tail.length
    }
  }

  def depth: Int = {
    this match {
      case Nil => 0
      case ACons(head, tail) => tail.depth
      case MCons(head, repeat, tail) => List(1+head.depth,tail.depth).max
    }
  }

  

  def foreach(f: T => Unit): Unit = {
    this.map(f)
  }

}

final case object Nil extends PList[Nothing] {}

final case class ACons[+T](head: T, tail: PList[T]) extends PList[T] {}

final case class MCons[T](head: PList[T], repeat: UInt, tail: PList[T]) extends PList[T] {}

trait PItem {
  def +[T >: this.type](that: T): PList[T] = {
    ACons(this, ACons(that, Nil))
  }

  def *(repeat: UInt): PList[this.type] = MCons(ACons(this, Nil), repeat, Nil)

  def *(repeat: Int): PList[this.type] = this*U(repeat)
}
