import scala.deriving.{Mirror}
import scala.compiletime.{constValueTuple, constValue, constValueOpt}

// from https://users.scala-lang.org/t/how-to-extract-a-list-of-field-names-from-a-product-using-mirrors/7724/4
inline def labelsOf[A](using p: Mirror.ProductOf[A]): p.MirroredElemLabels =
  constValueTuple[p.MirroredElemLabels]

case class Example(field1: Int, field2: Int)

labelsOf[(1, 2)]

val x1 = labelsOf[Example]
