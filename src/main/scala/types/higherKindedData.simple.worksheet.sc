case class Metadata(obtainedAt: Long, obtainedBy: String, obtainedFrom: String)
case class WithMetadata[T](meta: Metadata, value: T)

val meta = Metadata(1234567890, "obtainedBy", "obtainedFrom")

// ====================

case class Company[T[_]](
    name:      T[String],
    founded:   T[Int],
    employees: T[Vector[String]]
)

// -----------------

type OptionalCompany = Company[Option]

val c1 = Company(
  Some("ACME"),
  Some(2010),
  Some(Vector("John", "Jane"))
)

// -----------------

// type lambda https://docs.scala-lang.org/scala3/reference/new-types/type-lambdas.html
type MetadatedCompany = Company[[X] =>> WithMetadata[Option[X]]]

val c2_1 = Company(
  WithMetadata(meta, "ACME"),
  WithMetadata(meta, 2010),
  WithMetadata(meta, Vector("John", "Jane"))
)

val c2_2: MetadatedCompany = Company(
  WithMetadata(meta, Some("ACME")),
  WithMetadata(meta, Some(2010)),
  WithMetadata(meta, Some(Vector("John", "Jane")))
)

// -----------------

type CollectedCompany = Company[[X] =>> Vector[WithMetadata[Option[X]]]]

val c2_3: CollectedCompany = Company(
  Vector(WithMetadata(meta, Some("ACME"))),
  Vector(WithMetadata(meta, Some(2010))),
  Vector(WithMetadata(meta, Some(Vector("John", "Jane"))))
)


// ====================

