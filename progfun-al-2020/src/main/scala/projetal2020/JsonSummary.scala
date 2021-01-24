package projetal2020.JsonSummary
import play.api.libs.json._
import projetal2020.classes._
import projetal2020.enums._

object JsonSummary {

  implicit val gridWrites = new Writes[Grid] {
    def writes(grid: Grid) = Json.obj(
      "x" -> grid.x,
      "y" -> grid.y
    )
  }

  implicit val orientationWrites = new Writes[Orientation] {
    def writes(orientation: Orientation) = orientation match {
      case North => JsString(North.serialized.toString)
      case East  => JsString(East.serialized.toString)
      case South => JsString(South.serialized.toString)
      case West  => JsString(West.serialized.toString)
    }
  }

  implicit val instructionWrites = new Writes[Instruction] {
    def writes(instruction: Instruction) = instruction match {
      case Left    => JsString(Left.serialized.toString)
      case Right   => JsString(Right.serialized.toString)
      case Forward => JsString(Forward.serialized.toString)
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
