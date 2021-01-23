package projetal2020.JsonSummary
import play.api.libs.json._
import projetal2020.classes._
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
      Json.obj(
        "coordinates" -> Json.obj(
          "x" -> state.x,
          "y" -> state.y
        ),
        "orientation" -> state.orientation
      )
    }
  }

  implicit val mowerLifecycleWrites = new Writes[MowerLifecycle] {
    def writes(lifecycle: MowerLifecycle) = Json.obj(
      "initialState" -> lifecycle.initialState,
      "instructions" -> lifecycle.instructions,
      "finalState"   -> lifecycle.finalState
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
