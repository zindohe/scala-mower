package projetal2020.JsonSummary
import play.api.libs.json._
import projetal2020.classes._
import projetal2020.CoordinatesConverter._
import projetal2020.enums._
import Orientation._
import Instruction._

object JsonSummary {

  implicit val gridWrites = new Writes[Grid] {
    def writes(grid: Grid) = Json.obj(
      "x" -> grid.x,
      "y" -> grid.y
    )
  }

  implicit val orientationWrites = new Writes[Orientation] {
    def writes(orientation: Orientation) = orientation match {
      case North => JsString("N")
      case East  => JsString("E")
      case South => JsString("S")
      case West  => JsString("W")
    }
  }

  implicit val instructionWrites = new Writes[Instruction] {
    def writes(instruction: Instruction) = instruction match {
      case Left    => JsString("L")
      case Right   => JsString("R")
      case Forward => JsString("F")
    }
  }

  implicit val mowerStateWrites = new Writes[MowerState] {
    def writes(state: MowerState) = {
      val outputCoordinates =
        CoordinatesConverter.toOutputCoordinates((state.x, state.y))
      Json.obj(
        "coordinates" -> Json.obj(
          "x" -> outputCoordinates._1,
          "y" -> outputCoordinates._2
        ),
        "orientation" -> state.orientation
      )
    }
  }

  implicit val mowerLifecycleWrites = new Writes[MowerLifecycle] {
    def writes(lifecycle: MowerLifecycle) = Json.obj(
      "begin"        -> lifecycle.initialState,
      "instructions" -> lifecycle.instructions,
      "end"          -> lifecycle.finalState
    )
  }

  def generate(grid: Grid, mowersLifecycles: List[MowerLifecycle]): String =
    Json.prettyPrint(
      Json.toJson(
        Json.obj(
          "limits" -> grid,
          "mowers" -> mowersLifecycles
        )
      )
    )
}
