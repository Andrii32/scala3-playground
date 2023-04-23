case class Metadata(obtainedAt: Long, obtainedBy: String, obtainedFrom: String)
case class WithMetadata[T](meta: Metadata, value: T)

val meta = Metadata(1234567890, "obtainedBy", "obtainedFrom")

// ====================

class FCollected
class FMetadated
class FValidated
class FRaw

type Field[A, T] = A match
  case FRaw       => Option[T]
  case FMetadated => WithMetadata[Option[T]]
  case FCollected => Vector[WithMetadata[Option[T]]]

case class CompanyFields[T](
    name:      Field[T, String],
    founded:   Field[T, Int],
    employees: Field[T, Vector[String]]
)

val c3_1 = CompanyFields[FRaw](
  Some("ACME"),
  None,
  Some(Vector("John", "Jane"))
)

val c3_2 = CompanyFields[FMetadated](
  WithMetadata(meta, Some("ACME")),
  WithMetadata(meta, Some(2010)),
  WithMetadata(meta, Some(Vector("John", "Jane")))
)

val c3_3: CompanyFields[FCollected] = CompanyFields(
  Vector(WithMetadata(meta, Some("ACME"))),
  Vector(WithMetadata(meta, Some(2010))),
  Vector(WithMetadata(meta, Some(Vector("John", "Jane"))))
)

// ====================
