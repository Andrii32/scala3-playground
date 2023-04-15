import io.circe.syntax._
import io.circe.generic.auto._

case class Location(lat: Double, long: Double)

val location = Location(1.0, 2.0)

location.asJson

case class Address(
    country:  Option[String],
    city:     Option[String],
    street:   Option[String],
    location: Option[Location]
)

val address = Address(
  country = Some("Austria"),
  city = Some("Vienna"),
  street = None,
  location = Some(location)
)

address.asJson
// res1: Json = {
//   "country" : "Austria",
//   "city" : "Vienna",
//   "street" : null,
//   "location" : {
//     "lat" : 1.0,
//     "long" : 2.0
//   }
// }
