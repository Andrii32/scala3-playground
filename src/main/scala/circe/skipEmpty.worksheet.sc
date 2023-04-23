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

// ----------------------------
// drop null
address.asJson.dropNullValues
// res1: Json = {
//   "country" : "Austria",
//   "city" : "Vienna",
//   "locations" : [
//     {
//       "lat" : 1.0,
//       "long" : 2.0
//     }
//   ]
// }

// ----------------------------
// drop empty values

val emptyLocationsAddress = Address(
  country = Some("Austria"),
  city = Some("Vienna"),
  street = None,
  locations = Vector.empty
)

emptyLocationsAddress.asJson.dropEmptyValues
// res2: Json = {
//   "country" : "Austria",
//   "city" : "Vienna",
//   "street" : null
// }

// ----------------------------
// drop deep empty / drop deep null

case class Person(name: String, age: Int)

case class Company(name: String)

given Codec[Company] = deriveCodec
given Codec[Person] = deriveCodec

case class Document(
    name:             Option[String],
    address:          Option[Address],
    relatedCompanies: Vector[Company],
    employees:        Vector[Person]
)

given Codec[Document] = deriveCodec

val document = Document(
  name = None,
  address = Some(emptyLocationsAddress),
  relatedCompanies = Vector.empty,
  employees = Vector.empty
)

document.asJson.deepDropNullValues
// res3: Json = {
//   "address" : {
//     "country" : "Austria",
//     "city" : "Vienna",
//     "locations" : [
//     ]
//   },
//   "relatedCompanies" : [
//   ],
//   "employees" : [
//   ]
// }

// ----------------------------
// `deepDropEmptyValues` does not exist, but could be implemented like this:

import io.circe.{Json, JsonNumber, JsonObject}

def myDeepDropEmptyValues(x: Json): Json = {
  val folder = new Json.Folder[Json] {
    def onNull: Json = Json.Null
    def onBoolean(value: Boolean): Json = Json.fromBoolean(value)
    def onNumber(value:  JsonNumber): Json = Json.fromJsonNumber(value)
    def onString(value:  String): Json = Json.fromString(value)
    def onArray(value: Vector[Json]): Json =
      Json.fromValues(value.collect { case v =>
        v.foldWith(this)
      })
    def onObject(value: JsonObject): Json =
      Json
        .fromJsonObject(
          value.mapValues(_.foldWith(this))
        )
        .dropEmptyValues
  }

  x.foldWith(folder)
}

myDeepDropEmptyValues(document.asJson)
// res4: Json = {
//   "name" : null,
//   "address" : {
//     "country" : "Austria",
//     "city" : "Vienna",
//     "street" : null
//   }
// }

// ----------------------------
// or as extension method

extension (json: Json)
  def deepDropEmptyValues: Json = {
    val folder = new Json.Folder[Json] {
      def onNull: Json = Json.Null
      def onBoolean(value: Boolean): Json = Json.fromBoolean(value)
      def onNumber(value:  JsonNumber): Json = Json.fromJsonNumber(value)
      def onString(value:  String): Json = Json.fromString(value)
      def onArray(value: Vector[Json]): Json =
        Json.fromValues(value.collect { case v =>
          v.foldWith(this)
        })
      def onObject(value: JsonObject): Json =
        Json
          .fromJsonObject(
            value.mapValues(_.foldWith(this))
          )
          .dropEmptyValues
    }

    json.foldWith(folder)
  }

document.asJson.deepDropNullValues.deepDropEmptyValues
// res5: Json = {
//   "address" : {
//     "country" : "Austria",
//     "city" : "Vienna"
//   }
// }
