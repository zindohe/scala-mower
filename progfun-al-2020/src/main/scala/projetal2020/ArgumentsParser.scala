package projetal2020.ArgumentsParser

import annotation.tailrec

case class MowerProgramArguments(
    inputPath: Option[String],
    outputPath: Option[String]
)

object ArgumentsParser {

  def parseFromList(args: List[String]): MowerProgramArguments = {

    @tailrec
    def go(
        input: List[String],
        parsed: Map[String, String]
    ): Map[String, String] =
      input match {
        case key :: value :: rest =>
          go(rest, parsed + (key -> value))
        case _ => parsed
      }
    val argsMap = go(args, Map[String, String]())
    MowerProgramArguments(argsMap.get("--input"), argsMap.get("--output"))
  }
}
