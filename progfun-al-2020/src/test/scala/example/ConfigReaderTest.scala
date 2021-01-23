package projetal2020.test

import scala.util.{Failure, Success, Try}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterAll
import projetal2020.ConfigReader._
import projetal2020.classes._
import projetal2020.enums._
import Orientation._
import Instruction._
import projetal2020.exceptions._
import java.io._
import java.nio.file.Files
import java.nio.file.Paths

class ConfigReaderTest extends AnyFunSuite with BeforeAndAfterAll {

  private def isIncorrectDataExceptionFailure(t: Try[Config]): Boolean =
    t match {
      case Failure(e) =>
        e match {
          case _: IncorrectDataException => true
          case _                         => false
        }
      case _ => false
    }

  private val tempDir =
    Files.createTempDirectory("mower-config-reader-tests").toFile

  private val testFilePath = Paths.get(tempDir.toPath.toString + "/test.txt")

  override def beforeAll(): Unit = {
    super.beforeAll()
    val writer = new FileWriter(testFilePath.toString)
    writer.write("5 5\n")
    writer.write("1 3 N\n")
    writer.write("LFLF\n")
    writer.write("4 5 E\n")
    writer.write("RFFF\n")
    writer.close()
  }

  override def afterAll(): Unit = {
    super.afterAll()
    val file = new File(testFilePath.toString)
    // for some reason this does not work
    val _ = file.delete()
  }

  test("It should read a valid config from a file to a Success[Config]") {

    assert(
      ConfigReader.fromFile(testFilePath.toString) == Success(
        Config(
          Grid(5, 5),
          List(
            Mower(
              MowerState(1, 3, North),
              List(Left, Forward, Left, Forward)
            ),
            Mower(
              MowerState(4, 5, East),
              List(Right, Forward, Forward, Forward)
            )
          )
        )
      )
    )
  }

  test("It should read a valid config to a Success[Config]") {
    assert(
      ConfigReader.fromList(
        List(
          "5 5",
          "1 3 N",
          "LFLF",
          "4 5 E",
          "RFFF"
        )
      ) == Success(
        Config(
          Grid(5, 5),
          List(
            Mower(
              MowerState(1, 3, North),
              List(Left, Forward, Left, Forward)
            ),
            Mower(
              MowerState(4, 5, East),
              List(Right, Forward, Forward, Forward)
            )
          )
        )
      )
    )
  }

  test(
    "It should return Failure[IncorrectDataException] if grid size coordinates are negative"
  ) {

    assert(
      isIncorrectDataExceptionFailure(
        ConfigReader
          .fromList(
            List(
              "-5 5",
              "1 3 N",
              "LFLF",
              "4 5 E",
              "RFFF"
            )
          )
      )
    )
  }

  test(
    "It should return Failure[IncorrectDataException] if grid size coordinates are not numbers"
  ) {

    assert(
      isIncorrectDataExceptionFailure(
        ConfigReader
          .fromList(
            List(
              "a 5",
              "1 3 N",
              "LFLF",
              "4 5 E",
              "RFFF"
            )
          )
      )
    )
  }

  test(
    "It should return Failure[IncorrectDataException] if one of mowers' initial state coordinates are negative"
  ) {

    assert(
      isIncorrectDataExceptionFailure(
        ConfigReader
          .fromList(
            List(
              "5 5",
              "-1 3 N",
              "LFLF",
              "4 5 E",
              "RFFF"
            )
          )
      )
    )
  }

  test(
    "it should return Failure[IncorrectDataException] if instructions are not valid characters"
  ) {
    assert(
      isIncorrectDataExceptionFailure(
        ConfigReader
          .fromList(
            List(
              "5 5",
              "1 3 N",
              "BFLF",
              "4 5 E",
              "RFFF"
            )
          )
      )
    )
  }

  test(
    "it should return Failure[IncorrectDataException] if one of the mowers' orientation are not valid characters"
  ) {
    assert(
      isIncorrectDataExceptionFailure(
        ConfigReader
          .fromList(
            List(
              "5 5",
              "1 3 V",
              "BFLF",
              "4 5 E",
              "RFFF"
            )
          )
      )
    )
  }
}
