import scala.deriving.{Mirror}

import scala.compiletime.{summonInline, constValueTuple}

sealed trait Field[Label <: String, Type]

type FromLabelsAndTypes[Labels <: Tuple, Types <: Tuple] <: Tuple =
  (Labels, Types) match {
    case (EmptyTuple, EmptyTuple) => EmptyTuple
    case (labelHead *: labelTail, typeHead *: typeTail) =>
      Field[labelHead, typeHead] *: FromLabelsAndTypes[labelTail, typeTail]
  }

// ==================== labelsOf
// from https://users.scala-lang.org/t/how-to-extract-a-list-of-field-names-from-a-product-using-mirrors/7724/4
inline def labelsOf[A](using p: Mirror.ProductOf[A]): p.MirroredElemLabels =
  constValueTuple[p.MirroredElemLabels]

case class Example(field1: Int, field2: Int)

labelsOf[(1, 2)]

val x1 = labelsOf[Example]
// ====================

// ==================== sameLabels

import scala.compiletime.ops.boolean.&&
import scala.compiletime.ops.any.==

type SameLabels[Fields1 <: Tuple, Fields2 <: Tuple] <: Boolean =
  (Fields1, Fields2) match {
    case (EmptyTuple, EmptyTuple) => true
    case (Field[label1, _] *: tail1, Field[label2, _] *: tail2) =>
      label1 == label2 && SameLabels[tail1, tail2]
    case _ => false
  }

// inline + summonInline needed to make this work
inline def sameLabels[T1 <: Product, T2 <: Product](using
    m1: Mirror.ProductOf[T1],
    m2: Mirror.ProductOf[T2]
): Boolean = {
  summonInline[
    SameLabels[
      FromLabelsAndTypes[m1.MirroredElemLabels, m1.MirroredElemTypes],
      FromLabelsAndTypes[m2.MirroredElemLabels, m2.MirroredElemTypes]
    ] =:= true
  ]
  true
}

case class Company(age: Int, name: String)
case class CompanyA(age: Int, name: String, other: String)
case class Person(age: Int, name: String)

sameLabels[Company, Company]
sameLabels[Company, Person]

// ====================

// ==================== typed kv pairs
val companyMirror = summon[Mirror.ProductOf[Company]]

type KVPairs[Fields <: Tuple] <: Tuple =
  Fields match {
    case EmptyTuple                => EmptyTuple
    case Field[label, tpe] *: tail => (label, tpe) *: KVPairs[tail]
  }

val kvpairs1: KVPairs[FromLabelsAndTypes[
  companyMirror.MirroredElemLabels,
  companyMirror.MirroredElemTypes
]] = (("age", 1), ("name", "John"))
// val kvpairs2: KVPairs[FromLabelsAndTypes[companyMirror.MirroredElemLabels, companyMirror.MirroredElemTypes]] = (("age", 1), ("name", "John"), ("name", "John")) // error
// val kvpairs2: KVPairs[FromLabelsAndTypes[companyMirror.MirroredElemLabels, companyMirror.MirroredElemTypes]] = (("age1", 1), ("name1", "John")) // error

// ====================
