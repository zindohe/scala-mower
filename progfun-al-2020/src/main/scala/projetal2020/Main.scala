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

  for (element <- lawnmowers) {
    println(element)
  }

}

// object Direction extends Enumeration {
//   val N, E, W, S = Value

//   def getFromString(s: String): Option[Value] =
//     values.find(_.toString == s)

//   override def toString(): String = {
//     ""
//   }

// }

object Direction extends Enumeration {
  val North = Value("N")
  val East = Value("E")
  val West = Value("W")
  val South = Value("S")

  def getFromString(s: String): Option[Value] =
    values.find(_.toString == s)
}

object Instruction extends Enumeration {
  val Gauche = Value("G")
  val Droite = Value("D")
  val Avancer = Value("A")

  def getFromString(s: String): Option[Value] =
    values.find(_.toString == s)
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

  // def calcul_final_pos(lawnmower: LawnMower) = {

  // }

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
              (v: String) =>
                Instruction.getFromString(v).getOrElse(Instruction.Avancer)
            )
        )
      parse_one_lawnmower(rest, lawnmowers_list ::: List(new_lawnmower))
    }
    case _ => lawnmowers_list
  }

}

class LawnMower(
    val x: Int,
    val y: Int,
    val direction: Direction.Value,
    val instructions: Array[Instruction.Value]
) {
  override def toString(): String = {
    "x : " + x.toString +
      "\ny : " + y.toString +
      "\ndirection : " + direction.toString +
      "\ninstructions : " + instructions
      .map((v: Instruction.Value) => v.toString)
      .mkString("")
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
