package packUnpack

import spinal.core._
import spinal.lib._

case class PackTable(
  width: BitCount,
  table: Bits,
  longset: Option[UInt] = None
) extends Area with PackTrait {

  val long = longset.getOrElse(U(table.getWidth/width.value))

  val onMatch = Bool()

  val tableReg = RegInit(table)

  val output = Stream(Bits(width))
  output.valid := True
  output.payload := tableReg(width.value-1 downto 0)

  val onActive = onMatch && output.ready

  when(onActive) {
    tableReg := tableReg.rotateRight(width.value)
  }

}
