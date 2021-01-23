package projetal2020.enums

object Instruction extends Enumeration {
  type Instruction = Value
  val Left, Right, Forward = Value
}

object Orientation extends Enumeration {
  type Orientation = Value
  val North, East, South, West = Value
}
