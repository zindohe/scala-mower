package projetal2020.FileWriter

import java.io._
import scala.util.{Try}

object FileWriter {
  def write(filePath: String, data: String): Try[Unit] =
    Try({
      val file = new File(filePath)
      val writer = new BufferedWriter(new FileWriter(file))
      writer.write(data)
      writer.close()
    })
}
