package packUnpack

import spinal.core._
import spinal.lib._

trait PackTrait {

  val long: UInt

  val output: Stream[Bits]

  val onMatch: Bool
  val onActive: Bool

}
