final case class Coordinates private (
    lat: Double,
    lon: Double
)

object Coordinates {
  def fromValues(lat: Double, lon: Double): Option[Coordinates] =
    if (-90 <= lat && lat <= 90) && (-180 <= lon && lon <= 180)
    then new Some(Coordinates(lat, lon))
    else None
}


Coordinates.fromValues(1.0, 2.0) // Some(Coordinates(1.0, 2.0))
Coordinates.fromValues(1.0, 200.0) // None
