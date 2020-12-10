package projetal2020

object Main extends App {

  def grille = new Grid();
  def tondeuse = new LawnMower(0, 0, Direction.North, grille);
  tondeuse.move(Instruction.Avancer);
}

object Direction extends Enumeration {
  val North, East, West, South = Value
}

object Instruction extends Enumeration {
  val Gauche, Droite, Avancer = Value
}

class Grid {}

class LawnMower(
    val x: Int,
    val y: Int,
    val orientation: Direction.Value,
    val grid: Grid
) {
  // def positions: Array[Int] = Array();
  // def orientation: Direction.Value;
  // def grid: Grid;

  // def LawnMower(val x: Int, val y: Int, val orientation: Direction.Value, val3 grid: Grid) = {
  //   this.positions(0) = x;
  //   this.positions(1) = y;
  //   this.orientation = orientation;
  //   this.grid = grid;
  // }

  def move(instruction: Instruction.Value) = {
    println("instruction : " + instruction.toString);
  }
}

object FileHandler {}
