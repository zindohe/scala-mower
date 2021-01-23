package projetal2020
import scala.util.{Failure, Success}
import projetal2020.ConfigReader._
import projetal2020.FileWriter._
import projetal2020.JsonSummary._
import projetal2020.classes._

// I'm specifically asked to THROW an exception in the project guidelines, so i'm afraid i will have to do this.
@SuppressWarnings(Array("org.wartremover.warts.Throw"))
object Main extends App {
  ConfigReader.fromFile("instructions.txt") match {
    case Failure(exception) => {
      throw exception
    }
    case Success(config: Config) => {
      val finalStates = config.mowers.map(mower => {
        mower.execute(config.grid)
      })
      val lifecycles = config.mowers
        .zip(finalStates)
        .map(
          (zipped) =>
            MowerLifecycle(zipped._1.state, zipped._1.instructions, zipped._2)
        )
      val summary = JsonSummary.generate(config.grid, lifecycles)

      val outputFilePath = "summary.json"

      FileWriter.write(outputFilePath, summary) match {
        case Failure(e) => {
          val msg = e.getMessage()
          println(s"Failure while writing output to $outputFilePath ($msg).")
        }
        case _ =>
          println(s"All done. Output was saved to $outputFilePath.")
      }
    }
  }
}
