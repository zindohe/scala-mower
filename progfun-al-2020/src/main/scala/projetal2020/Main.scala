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
}

object Direction extends Enumeration {
  val North, East, West, South = Value

  //def getString(s: String): Option[Value] = values.find(_.toString == s)

}

object Instruction extends Enumeration {
  val Gauche, Droite, Avancer = Value
}

class Grid(val height: Int, val width: Int) {}

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

  // def move(
  //     x: Int,
  //     y: Int,
  //     orientation: Direction.Value,
  //     instructions: Array[String]
  // ) = {}

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
          starting_stats(2),
          m
        )
      parse_one_lawnmower(rest, lawnmowers_list ::: List(new_lawnmower))
    }
    case _   => lawnmowers_list
  }

}

class LawnMower(
    val x: Int,
    val y: Int,
    val direction: String,
    val instructions: String
) {
  override def toString(): String = {
    "x : " + x.toString +
      "\ny : " + y.toString +
      "\ndirection : " + direction +
      "\ninstructions : " + instructions
  }
}

object FileHandler {
  def read_instructions(filename: String) = {
    Source.fromFile(filename).getLines().map((elem: String) => elem)
  }
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
