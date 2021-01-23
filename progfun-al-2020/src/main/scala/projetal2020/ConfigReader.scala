package projetal2020.ConfigReader

import scala.util.{Failure, Success, Try}
import scala.io.Source
import annotation.tailrec
import projetal2020.exceptions._
import projetal2020.classes._
import projetal2020.enums._
import Orientation._
import Instruction._

object ConfigReader {

  private def parsePositiveInt(string: String): Try[Int] =
    Try(string.toInt).flatMap(int => {
      if (int >= 0) {
        Success(int)
      } else {
        Failure(new IncorrectDataException("Bad positive integer string."))
      }
    })

  private def parseCoordinates(
      xString: String,
      yString: String
  ): Try[(Int, Int)] =
    (this.parsePositiveInt(xString), this.parsePositiveInt(yString)) match {
      case (Success(x), Success(y)) => Success((x, y))
      case _ =>
        Failure(
          new IncorrectDataException(
            "Coordinates members must be positive integers."
          )
        )
    }

  private def parseOrientation(
      orientationString: String
  ): Try[Orientation] =
    orientationString match {
      case "N" => Success(North)
      case "E" => Success(East)
      case "S" => Success(South)
      case "W" => Success(West)
      case _ =>
        Failure(
          new IncorrectDataException(
            s"Orientation $orientationString is invalid. Must be either N, E, S or W."
          )
        )
    }

  private def parseGrid(line: String): Try[Grid] = {

    val splited = line.split(" ")
    (splited.headOption, splited.drop(1).headOption) match {
      case (Some(xString), Some(yString)) =>
        parseCoordinates(xString, yString).flatMap(
          coordinates => Success(Grid(coordinates._1, coordinates._2))
        )
      case _ =>
        Failure(
          new IncorrectDataException(
            "Grid size members must be two elements separated by a single space."
          )
        )
    }
  }

  private def parseMowerState(line: String): Try[MowerState] = {
    val splited = line.split(" ")
    (splited.headOption, splited.drop(1).headOption, splited.drop(2).headOption) match {
      case (Some(xString), Some(yString), Some(orientationString)) => {
        parseCoordinates(xString, yString).flatMap(coordinates => {
          parseOrientation(orientationString).map(
            orientation =>
              MowerState(coordinates._1, coordinates._2, orientation)
          )
        })
      }
      case _ =>
        Failure(
          new IncorrectDataException(
            "Bad mower state data in config."
          )
        )
    }
  }

  private def parseMowerInstructions(
      line: String
  ): Try[List[Instruction]] = {

    @tailrec
    def go(
        chars: List[Char],
        instructions: List[Instruction]
    ): Try[List[Instruction]] = chars match {
      case char :: tail =>
        char match {
          case 'L' => go(tail, instructions :+ Left)
          case 'R' => go(tail, instructions :+ Right)
          case 'F' => go(tail, instructions :+ Forward)
          case _ =>
            Failure(
              new IncorrectDataException(
                "Bad instructions. Valid instruction characters are L, R and F."
              )
            )
        }
      case _ => Success(instructions)
    }

    go(line.toList, List())
  }

  private def parseMowerConfigs(lines: List[String]): Try[List[Mower]] = {

    @tailrec
    def go(
        lines: List[String],
        configs: List[Mower]
    ): Try[List[Mower]] = lines match {
      case initString :: instructionsString :: rest =>
        this
          .parseMowerState(initString)
          .flatMap(state => {
            this
              .parseMowerInstructions(instructionsString)
              .map((instructions) => (state, instructions))
          }) match {
          case Success(results) =>
            go(rest, configs :+ Mower(results._1, results._2))
          case Failure(e) => Failure(e)
        }
      case _ => Success(configs)
    }

    go(lines, List())
  }

  def fromList(lines: List[String]): Try[Config] =
    lines.headOption match {
      case Some(firstLine: String) =>
        this
          .parseGrid(firstLine)
          .flatMap(
            grid =>
              this
                .parseMowerConfigs(lines.drop(1))
                .map(mowerConfigs => Config(grid, mowerConfigs))
          ) match {
          case Failure(e: Throwable) => Failure(e)
          case success               => success
        }
      case None =>
        Failure(
          new IncorrectDataException(
            "Could not read grid size line from config"
          )
        )
    }

  def fromFile(filePath: String): Try[Config] =
    Try(Source.fromFile(filePath).getLines().toList) match {
      case Success(lines: List[String]) =>
        ConfigReader.fromList(lines)

      case Failure(e) =>
        Failure(
          new IncorrectDataException(
            s"File $filePath could not be read (${e.getMessage}). Please verify file path."
          )
        )
    }
}
