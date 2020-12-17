package projetal2020
import scala.io.Source
import play.api.libs.json._
import java.io._

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
object Main extends App {

  val configList = FileHandler.readInstructions("instructions.txt").toList

  try {
    val grid = Executor.parseGridSize(
      configList.headOption
        .getOrElse(throw new IncorrectDataException("Can't parse input data"))
    )
    val lawnmowers = Executor.parseLawnmowers(configList.drop(1))

    val results = lawnmowers.map(
      (l: LawnMower) =>
        (
          l,
          Executor
            .calculFinalPos(
              l.coordinates,
              l.instructions,
              grid
            )
        )
    )

    FileHandler.writeResults(
      "resultat.json",
      FileHandler.getJson(results, grid)
    )
  } catch {
    case e: Exception => println(e.getMessage)
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

  def calculFinalPos(
      result: Coordinates,
      instructions: List[Instruction.Value],
      grid: Grid
  ): Coordinates = instructions match {
    case value :: rest if value == Instruction.Avancer =>
      calculFinalPos(
        checkCoordinate(result, Executor.moveForward(result), grid),
        rest,
        grid
      )
    case value :: rest =>
      calculFinalPos(
        checkCoordinate(
          result,
          new Coordinates(
            result.x,
            result.y,
            Direction.getNewDirection(result.direction, value)
          ),
          grid
        ),
        rest,
        grid
      )
    case Nil => result
  }

  def checkCoordinate(
      currentCoordinates: Coordinates,
      newCoordinates: Coordinates,
      grid: Grid
  ): Coordinates = newCoordinates match {
    case n if grid.checkCoordinate(n) => newCoordinates
    case _                            => currentCoordinates
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def parseGridSize(config: String) = {
    def sizes = config.split(" ")
    if (sizes.size != 2) {
      throw new IncorrectDataException("Invalid grid size")
    } else {
      try {
        new Grid(sizes(0).toInt, sizes(1).toInt)
      } catch {
        case _: Exception =>
          throw new IncorrectDataException("Grid size are not numbers")
      }
    }
  }

  def parseLawnmowers(lawnmowers: List[String]) = {
    parseOneLawnmower(lawnmowers, List[LawnMower]())
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def parseOneLawnmower(
      lawnmowers: List[String],
      lawnmowersList: List[LawnMower]
  ): List[LawnMower] = lawnmowers match {
    case n :: m :: rest => {
      def startingStats = n.split(" ")
      if (startingStats.size != 3) {
        throw new IncorrectDataException(
          "Invalid starting position for a lawnmower"
        )
      }
      try {
        def newLawnmower =
          new LawnMower(
            new Coordinates(
              startingStats(0).toInt,
              startingStats(1).toInt,
              Direction
                .getFromString(startingStats(2))
                .getOrElse(
                  throw new IncorrectDataException("Invalid Direction")
                )
            ),
            m.split("")
              .map(
                Instruction
                  .getFromString(_)
                  .getOrElse(
                    throw new IncorrectDataException("Invalid Instructions")
                  )
              )
              .toList
          )
        parseOneLawnmower(rest, lawnmowersList ::: List(newLawnmower))
      } catch {
        case n: IncorrectDataException => throw n
        case _: Exception =>
          throw new IncorrectDataException(
            "Starting position coordinates are not numbers"
          )
      }
    }
    case _ => lawnmowersList
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
    val coordinates: Coordinates,
    val instructions: List[Instruction.Value]
) {

  override def toString(): String = {
    coordinates.toString +
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
  def readInstructions(filename: String) = {
    Source.fromFile(filename).getLines().map((elem: String) => elem)
  }

  implicit val coordinatesWrite = new Writes[Coordinates] {
    def writes(coordinate: Coordinates) = Json.obj(
      "point" -> Json.obj(
        "x" -> coordinate.x,
        "y" -> coordinate.y
      ),
      "direction" -> coordinate.direction.toString
    )
  }

  def getJson(results: List[(LawnMower, Coordinates)], grid: Grid) = {
    Json.prettyPrint(
      Json.toJson(
        Json.obj(
          "limite" -> Json.obj(
            "x" -> grid.width,
            "y" -> grid.height
          ),
          "tondeuses" -> results.map(
            lawnmower =>
              Json.obj(
                "debut"        -> lawnmower._1.coordinates,
                "instructions" -> lawnmower._1.instructions,
                "fin"          -> lawnmower._2
              )
          )
        )
      )
    )
  }

  def writeResults(filename: String, results: String) = {
    val file = new File(filename)
    val buffer = new BufferedWriter(new FileWriter(file))
    buffer.write(results.toString)
    buffer.close()
  }
}

class IncorrectDataException(
    message: String
) extends RuntimeException(message) {}
