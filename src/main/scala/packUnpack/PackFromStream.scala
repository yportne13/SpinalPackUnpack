package packUnpack

import spinal.core._
import spinal.lib._

case class PackFromStream[T <: Data](
  width: BitCount,
  long: UInt
) extends Area with PackTrait {

  val onMatch = Bool()

  val source = Stream(Bits(width))

  val output = Stream(Bits(width))
  output.valid := source.valid
  output.payload := source.payload

  source.ready := onMatch && output.ready

  val onActive = onMatch && source.valid && output.ready

  def from(data: Stream[Bits]) = {
    data >> source
  }
  
}
