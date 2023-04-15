import io.circe.Json

val jsonString = Json.fromString("test")
val jsonNumber = Json.fromInt(42)
val jsonArray = Json.arr(jsonString, jsonNumber)
val jsonObject = Json.obj("key" -> jsonString, "key2" -> jsonNumber)
val jsonBoolean = Json.fromBoolean(true)
val jsonNull = Json.Null

// using implicit syntax
import io.circe.syntax._

val jsonString2 = "test".asJson
val jsonNumber2 = 42.asJson
val jsonArray2 = List(jsonString2, jsonNumber2).asJson
val jsonObject2 = Map("key" -> jsonString2, "key2" -> jsonNumber2).asJson
val jsonBoolean2 = true.asJson
val jsonNull2 = None.asJson
