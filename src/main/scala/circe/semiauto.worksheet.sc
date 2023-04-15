import io.circe.syntax._
import io.circe.{Encoder, Decoder, Codec}
import io.circe.generic.semiauto.{deriveEncoder, deriveDecoder, deriveCodec}

case class Location(lat: Double, long: Double)

case class Address(
    country:   Option[String],
    city:      Option[String],
    street:    Option[String],
    locations: Vector[Location]
)

// encoder / decoder
given locationDecoder: Decoder[Location] = deriveDecoder
given locationEncoder: Encoder[Location] = deriveEncoder

// or codec
given addressCodec: Codec[Address] = deriveCodec

// examples
val location = Location(1.0, 2.0)
val address = Address(
  country = Some("Austria"),
  city = Some("Vienna"),
  street = None,
  locations = Vector(location)
)

address.asJson
// res0: Json = {
//   "country" : "Austria",
//   "city" : "Vienna",
//   "street" : null,
//   "locations" : [
//     {
//       "lat" : 1.0,
//       "long" : 2.0
//     }
//   ]
// }
