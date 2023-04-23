case class Metadata(obtainedAt: Long, obtainedBy: String, obtainedFrom: String)
case class WithMetadata[T](meta: Metadata, value: T)

val meta = Metadata(1234567890, "obtainedBy", "obtainedFrom")

// ====================

class FCollected
class FMetadated
class FSchema
class FRaw

case class Named[N]()(implicit v: ValueOf[N]) {
  val fName = v.value
}

type Field[A, N, T] = A match
  case FRaw       => Option[T]
  case FMetadated => WithMetadata[Option[T]]
  case FCollected => Vector[WithMetadata[Option[T]]]
  case FSchema    => Named[N]

case class CompanyFields[A](
    name:      Field[A, "name", String],
    founded:   Field[A, "founded", Int],
    employees: Field[A, "employees", Vector[String]]
)

val schema = CompanyFields[FSchema](Named(), Named(), Named())

println(schema.name.fName)
println(schema.founded.fName)
println(schema.employees.fName)

// ====================
