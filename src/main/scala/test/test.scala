package test

import spinal.core._
import spinal.lib._

import packUnpack._

object test extends App {
  class toplevel extends Component {
    val io = new Bundle {
      val start = in Bool()
      val input = slave Stream(Bits(8 bits))
      val output = master Stream(Bits(8 bits))
    }

    val idle = new PackIdle(8 bits)
    val head1 = new PackTable(8 bits, B("0000000111111111"))//B(256, 16 bits))
    val head2 = new PackTable(8 bits, B("000000100000001000000000"))
    val data = new PackFromStream(8 bits, U(11))
    data.from(io.input)

    val x = List(1,2)

    val list: PList[PackTrait] = idle + head1 + (head2 + data)*3

    io.output << Pack(list)(io.start).m2sPipe()

  }

  SpinalVerilog(new toplevel)

  import spinal.core.sim._
  import spinal.lib._
  SimConfig.withWave.doSim(new toplevel){dut =>
    dut.clockDomain.forkStimulus(10)
    for(idx <- 0 until 100) {
      if(idx == 12) {
        dut.io.start #= true
      }else {
        dut.io.start #= false
      }
      dut.io.input.valid #= (idx%3)<2
      dut.io.input.payload #= idx
      dut.io.output.ready #= true
      dut.clockDomain.waitSampling()
    }
  }

}
