# Spinal Pack Unpack Lib

a dsl to generate pack and unpack

not finished yet

## example

```scala
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

  val list = idle + head1 + (head2 + data)*3

  io.output << Pack(list)(io.start).m2sPipe()

}
```
