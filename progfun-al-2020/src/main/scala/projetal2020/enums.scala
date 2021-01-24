package projetal2020.enums

sealed trait SerializableEnumeration {
  val serialized: Char
}

sealed trait SerializableEnumerationStatic[T] {
  def deserialize(char: Char): Option[T]
}

sealed trait Instruction extends SerializableEnumeration

object Instruction extends SerializableEnumerationStatic[Instruction] {
  override def deserialize(char: Char): Option[Instruction] = char match {
    case Left.serialized    => Some(Left)
    case Right.serialized   => Some(Right)
    case Forward.serialized => Some(Forward)
    case _                  => None
  }
}

case object Left extends Instruction {
  override val serialized = 'L'
}

case object Right extends Instruction {
  override val serialized = 'R'
}

case object Forward extends Instruction {
  override val serialized = 'F'
}

sealed trait Orientation extends SerializableEnumeration

object Orientation extends SerializableEnumerationStatic[Orientation] {
  override def deserialize(char: Char): Option[Orientation] = char match {
    case North.serialized => Some(North)
    case East.serialized  => Some(East)
    case South.serialized => Some(South)
    case West.serialized  => Some(West)
    case _                => None
  }
}

case object North extends Orientation {
  override val serialized = 'N'
}

case object East extends Orientation {
  override val serialized = 'E'
}
case object South extends Orientation {
  override val serialized = 'S'
}

case object West extends Orientation {
  override val serialized = 'W'
}
