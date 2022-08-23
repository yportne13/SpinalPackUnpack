package packUnpack

import spinal.core._
import spinal.lib._

trait PackTrait {

  val long: UInt

  val output: Stream[Bits]

  val onMatch: Bool
  val onActive: Bool

  def +(that: PackTrait): PList[PackTrait] = {
    ACons(this, ACons(that, Nil))
  }

  //def +[T <: PList](that: T): PList = {
  //  ACons(this, that)
  //}

  def *(repeat: UInt): PList[this.type] = MCons(ACons(this, Nil), repeat, Nil)

  def *(repeat: Int): PList[this.type] = this*U(repeat)

}
