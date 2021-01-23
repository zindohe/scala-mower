package projetal2020.test

import play.api.libs.json._
import play.api.libs.json.Reads._
import projetal2020.classes._
import projetal2020.enums._
import Instruction._
import Orientation._
import projetal2020.JsonSummary._
import org.scalatest.funsuite.AnyFunSuite

final case class Coordinates(x: Int, y: Int)

@SuppressWarnings(Array("org.wartremover.warts.Any"))
class JsonSummaryTest extends AnyFunSuite {

  test("Should generate a JSON summary") {

    val grid = Grid(5, 5)
    val lifecycles = List(
      MowerLifecycle(
        MowerState(1, 2, North),
        List(
          Left,
          Forward,
          Left,
          Forward,
          Left,
          Forward,
          Left,
          Forward,
          Forward
        ),
        MowerState(1, 3, North)
      ),
      MowerLifecycle(
        MowerState(3, 3, East),
        List(
          Forward,
          Forward,
          Right,
          Forward,
          Forward,
          Right,
          Forward,
          Right,
          Right,
          Forward
        ),
        MowerState(5, 1, East)
      )
    )

    val json = JsonSummary.generate(grid, lifecycles)

    val parsed = Json.parse(json)

    val limitsReads: List[Reads[Int]] = List(
      (JsPath \ "limits" \ "x").read[Int],
      (JsPath \ "limits" \ "y").read[Int]
    )

    limitsReads.foreach(
      reads =>
        assert(parsed.validate[Int](reads) match {
          case _: JsError        => false
          case e: JsSuccess[Int] => e.get == 5
        })
    )

    def checkMowerLifecycle(
        index: Int,
        lifecycle: MowerLifecycle
    ): Boolean = {

      def checkInt(res: JsResult[Int], expected: Int): Boolean = res match {
        case _: JsError        => false
        case e: JsSuccess[Int] => e.get == expected
      }

      def checkOrientation(
          res: JsResult[String],
          expected: Orientation
      ): Boolean =
        res match {
          case _: JsError => false
          case e: JsSuccess[String] =>
            e.get match {
              case "N" => expected == North
              case "E" => expected == East
              case "S" => expected == South
              case "W" => expected == West
              case _   => false
            }
        }

      def checkInstructions(
          res: JsResult[List[String]],
          expectedList: List[Instruction]
      ): Boolean = res match {
        case _: JsError => false
        case e: JsSuccess[List[String]] => {
          val jsonList = e.get
          jsonList
            .zip(expectedList)
            .forall(t => {
              val jsonString = t._1
              val expected = t._2
              jsonString match {
                case "F" => expected == Forward
                case "L" => expected == Left
                case "R" => expected == Right
                case _   => false
              }
            })
        }
      }

      val coordinatesCheck = List(
        (
          (JsPath \ "mowers" \ (index) \ "initialState" \ "coordinates" \ "x"),
          lifecycle.initialState.x
        ),
        (
          (JsPath \ "mowers" \ (index) \ "initialState" \ "coordinates" \ "y"),
          lifecycle.initialState.y
        ),
        (
          (JsPath \ "mowers" \ (index) \ "finalState" \ "coordinates" \ "x"),
          lifecycle.finalState.x
        ),
        (
          (JsPath \ "mowers" \ (index) \ "finalState" \ "coordinates" \ "y"),
          lifecycle.finalState.y
        )
      ).forall(check => {
        val path = check._1
        val value = check._2
        checkInt(
          parsed
            .validate[Int](
              path
                .read[Int]
            ),
          value
        )
      })

      val orientationChecks = List(
        (
          (JsPath \ "mowers" \ (index) \ "initialState" \ "orientation"),
          lifecycle.initialState.orientation
        ),
        (
          (JsPath \ "mowers" \ (index) \ "finalState" \ "orientation"),
          lifecycle.finalState.orientation
        )
      ).forall(check => {
        val path = check._1
        val value = check._2
        checkOrientation(
          parsed
            .validate[String](
              path
                .read[String]
            ),
          value
        )
      })

      val instructionsCheck = checkInstructions(
        parsed
          .validate[List[String]](
            (JsPath \ "mowers" \ (index) \ "instructions").read[List[String]]
          ),
        lifecycle.instructions
      )

      coordinatesCheck && orientationChecks && instructionsCheck
    }

    lifecycles.zipWithIndex
      .foreach((zipped) => {
        val lifecycle = zipped._1
        val i = zipped._2
        assert(checkMowerLifecycle(i, lifecycle))
      })

  }
}
