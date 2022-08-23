package packUnpack

import spinal.core._
import spinal.lib._

case class PackIdle(
  width: BitCount,
) extends Area with PackTrait {

  val long: UInt = U(0)

  val onMatch = Bool()

  val output = Stream(Bits(width))
  output.valid := False
  output.payload := 0

  val onActive = False

}
