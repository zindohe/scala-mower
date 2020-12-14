package projetal2020
import scala.io.Source

object Main extends App {
  //Executor.start_move(0, 0, Direction.North, "AAAGGDDAA");

  val configList = FileHandler.read_instructions("instructions.txt").toList

  // for (element <- configList) {
  //   println(element)
  // }

  val grid = Executor.parse_grid_size(configList.headOption.getOrElse("4 4"))
  // println("height : " + grid.height.toString)
  // println("width : " + grid.width.toString)

  val lawnmowers = Executor.parse_lawnmowers(configList.drop(1))

  // for (element <- lawnmowers) {
  //   println(element)
  // }

  val results = lawnmowers.map(
    (l: LawnMower) =>
      Executor
        .calcul_final_pos(
          new Coordinates(l.x, l.y, l.direction),
          l.instructions
        )
  )

  for (result <- results) {
    println(result.toString)
  }

}

object Direction extends Enumeration {
  val North = Value("N")
  val East = Value("E")
  val West = Value("W")
  val South = Value("S")

  def getFromString(s: String): Option[Value] =
    values.find(_.toString == s)

  def getNewDirection(
      current: Direction.Value,
      instruction: Instruction.Value
  ) = current match {
    case n if n == Direction.North && instruction == Instruction.Droite =>
      Direction.East
    case n if n == Direction.East && instruction == Instruction.Droite =>
      Direction.South
    case n if n == Direction.South && instruction == Instruction.Droite =>
      Direction.West
    case n if n == Direction.West && instruction == Instruction.Droite =>
      Direction.North
    case n if n == Direction.North && instruction == Instruction.Gauche =>
      Direction.West
    case n if n == Direction.West && instruction == Instruction.Gauche =>
      Direction.South
    case n if n == Direction.South && instruction == Instruction.Gauche =>
      Direction.East
    case n if n == Direction.East && instruction == Instruction.Gauche =>
      Direction.North
  }
}

object Instruction extends Enumeration {
  val Gauche = Value("G")
  val Droite = Value("D")
  val Avancer = Value("A")

  def getFromString(s: String): Option[Value] =
    values.find(_.toString == s)

}

class Grid(val height: Int, val width: Int) {
  def checkCoordinate(coordinate: Coordinates): Boolean = coordinate match {
    case n if n.x < 0 || n.x > width  => false
    case n if n.y < 0 || n.y > height => false
    case _                            => true
  }
}

object Executor {

  def start_move(
      x: Int,
      y: Int,
      orientation: Direction.Value,
      instructions: String
  ) = {
    println(x.toString + y.toString + orientation.toString);
    instructions.split("");
  }

  def calcul_final_pos(
      result: Coordinates,
      instructions: List[Instruction.Value]
  ): Coordinates = instructions match {
    case value :: rest =>
      value match {
        case n if n == Instruction.Avancer =>
          calcul_final_pos(Executor.moveForward(result), rest)
        case n =>
          calcul_final_pos(
            new Coordinates(
              result.x,
              result.y,
              Direction.getNewDirection(result.direction, n)
            ),
            rest
          )
      }
    case Nil => result
  }

  def parse_grid_size(config: String) = {
    def sizes = config.split(" ")
    new Grid(sizes(0).toInt, sizes(1).toInt)
  }

  def parse_lawnmowers(lawnmowers: List[String]) = {
    parse_one_lawnmower(lawnmowers, List[LawnMower]())
  }

  def parse_one_lawnmower(
      lawnmowers: List[String],
      lawnmowers_list: List[LawnMower]
  ): List[LawnMower] = lawnmowers match {
    case n :: m :: rest => {
      def starting_stats = n.split(" ")
      def new_lawnmower =
        new LawnMower(
          starting_stats(0).toInt,
          starting_stats(1).toInt,
          Direction.getFromString(starting_stats(2)).getOrElse(Direction.North),
          m.split("")
            .map(
              Instruction.getFromString(_).getOrElse(Instruction.Avancer)
            )
            .toList
        )
      parse_one_lawnmower(rest, lawnmowers_list ::: List(new_lawnmower))
    }
    case _ => lawnmowers_list
  }

  def moveForward(coordinates: Coordinates) = coordinates match {
    case n if n.direction == Direction.North =>
      new Coordinates(n.x, n.y + 1, n.direction)
    case n if n.direction == Direction.East =>
      new Coordinates(n.x + 1, n.y, n.direction)
    case n if n.direction == Direction.South =>
      new Coordinates(n.x, n.y - 1, n.direction)
    case n if n.direction == Direction.West =>
      new Coordinates(n.x - 1, n.y, n.direction)
  }
}

class LawnMower(
    val x: Int,
    val y: Int,
    val direction: Direction.Value,
    val instructions: List[Instruction.Value]
) {

  override def toString(): String = {
    "x : " + x.toString +
      "\ny : " + y.toString +
      "\ndirection : " + direction.toString +
      "\ninstructions : " + instructions
      .map(_.toString)
      .mkString("")
  }
}

class Coordinates(
    val x: Int,
    val y: Int,
    val direction: Direction.Value
) {

  override def toString(): String = {
    "x : " + x.toString +
      "\ny : " + y.toString +
      "\ndirection : " + direction.toString
  }
}

object FileHandler {
  def read_instructions(filename: String) = {
    Source.fromFile(filename).getLines().map((elem: String) => elem)
  }

  // def write_results(filename: String) = {

  // }
}
// class LawnMower(
//     val x: Int,
//     val y: Int,
//     val orientation: Direction.Value,
//     val grid: Grid
// ) {

//   def move(instruction: Instruction.Value) = {
//     println("instruction : " + instruction.toString);
//   }
// }
