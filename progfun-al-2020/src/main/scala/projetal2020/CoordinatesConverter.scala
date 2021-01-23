package projetal2020.CoordinatesConverter

object CoordinatesConverter {
  def toInputCoordinates(coordinates: (Int, Int)) =
    (coordinates._1 - 1, coordinates._2 - 1)
  def toOutputCoordinates(coordinates: (Int, Int)) =
    (coordinates._1 + 1, coordinates._2 + 1)
}
