import scala.deriving.{Mirror}

case class Const()
summon[Mirror.Of[Const]]
// res0: Product {
//   type MirroredMonoType >: Const <: Const
//   type MirroredType >: Const <: Const
//   type MirroredLabel >: "Const" <: "Const"
//   type MirroredElemTypes >: EmptyTuple <: EmptyTuple
//   type MirroredElemLabels >: EmptyTuple <: EmptyTuple
// } = Const

summon[Mirror.ProductOf[Const]]
// res0: Product {
//   type MirroredMonoType >: Const <: Const
//   type MirroredType >: Const <: Const
//   type MirroredLabel >: "Const" <: "Const"
//   type MirroredElemTypes >: EmptyTuple <: EmptyTuple
//   type MirroredElemLabels >: EmptyTuple <: EmptyTuple
// } = Const

enum Tree[T] {
  case Leaf(value: T)
  case Branch(left: Tree[T], right: Tree[T])
}
summon[Mirror.SumOf[Tree[Int]]]
// res3: Sum {
//   type MirroredMonoType >: Tree[Int] <: Tree[Int]
//   type MirroredType >: Tree[Int] <: Tree[Int]
//   type MirroredLabel >: "Tree" <: "Tree"
//   type MirroredElemTypes >: *:[Leaf[Int], *:[Branch[Int], EmptyTuple]] <: *:[Leaf[Int], *:[Branch[Int], EmptyTuple]]
//   type MirroredElemLabels >: *:["Leaf", *:["Branch", EmptyTuple]] <: *:["Leaf", *:["Branch", EmptyTuple]]
// } = repl.MdocSession$MdocApp$Tree$@427862d8
summon[Mirror.SumOf[Tree[Int]]].ordinal(Tree.Leaf(3))
summon[Mirror.SumOf[Tree[Int]]].ordinal(Tree.Branch(Tree.Leaf(3), Tree.Leaf(4)))

case class Person(age: Int, name: String)
summon[Mirror.ProductOf[Person]]
// res5: Product {
//   type MirroredMonoType >: Company <: Company
//   type MirroredType >: Company <: Company
//   type MirroredLabel >: "Company" <: "Company"
//   type MirroredElemTypes >: *:[Int, *:[String, EmptyTuple]] <: *:[Int, *:[String, EmptyTuple]]
//   type MirroredElemLabels >: *:["age", *:["name", EmptyTuple]] <: *:["age", *:["name", EmptyTuple]]
// } = Company

summon[Mirror.ProductOf[Person]].fromProduct((37, "Lukas"))
summon[Mirror.ProductOf[Person]].fromProduct((37, "Lukas", "Sutter", 3))

// summon[Mirror.ProductOf[Company]].fromProductTyped((37, "Lukas"))  // experimental

// ====================
// based on https://scalac.io/blog/inline-your-boilerplate-harnessing-scala3-metaprogramming-without-macros/

sealed trait Field[Label <: String, Type]

type FromLabelsAndTypes[Labels <: Tuple, Types <: Tuple] <: Tuple =
  (Labels, Types) match {
    case (EmptyTuple, EmptyTuple) => EmptyTuple
    case (labelHead *: labelTail, typeHead *: typeTail) =>
      Field[labelHead, typeHead] *: FromLabelsAndTypes[labelTail, typeTail]
  }

case class Company(age: Int, name: String)
val companyMirror = summon[Mirror.ProductOf[Company]]
// res5: Product {
//   type MirroredMonoType >: Company <: Company
//   type MirroredType >: Company <: Company
//   type MirroredLabel >: "Company" <: "Company"
//   type MirroredElemTypes >: *:[Int, *:[String, EmptyTuple]] <: *:[Int, *:[String, EmptyTuple]]
//   type MirroredElemLabels >: *:["age", *:["name", EmptyTuple]] <: *:["age", *:["name", EmptyTuple]]
// } = Company

type Fields = FromLabelsAndTypes[
  companyMirror.MirroredElemLabels,
  companyMirror.MirroredElemTypes
]
summon[Fields =:= Field["age", Int] *: Field["name", String] *: EmptyTuple]

type TypeForLabel[Label, Fields <: Tuple] =
  Fields match {
    case EmptyTuple =>
      Nothing // there is explanation in blogpost, why Nothing is used, and addtional code to make nicer error message
    case Field[Label, tpe] *: tail => tpe
    case head *: tail              => TypeForLabel[Label, tail]
  }

summon[TypeForLabel["age", Fields] =:= Int]
summon[TypeForLabel["name", Fields] =:= String]
summon[TypeForLabel["notExist", Fields] =:= Nothing]

type DropByLabel[Label, Fields <: Tuple] <: Tuple =
  Fields match {
    case EmptyTuple                => EmptyTuple
    case Field[Label, tpe] *: tail => tail
    case head *: tail              => head *: DropByLabel[Label, tail]
  }

type FieldsWithoutAge = DropByLabel["age", Fields]
summon[FieldsWithoutAge =:= Field["name", String] *: EmptyTuple]
summon[DropByLabel["name", FieldsWithoutAge] =:= EmptyTuple]

import scala.compiletime.ops.boolean.&&
import scala.compiletime.ops.any.==

type SameLabels[Fields1 <: Tuple, Fields2 <: Tuple] <: Boolean =
  (Fields1, Fields2) match {
    case (EmptyTuple, EmptyTuple) => true
    case (Field[label1, _] *: tail1, Field[label2, _] *: tail2) =>
      label1 == label2 && SameLabels[tail1, tail2]
    case _ => false
  }

summon[SameLabels[Fields, Fields] =:= true]
summon[SameLabels[Fields, DropByLabel["age", Fields]] =:= false]

type HasLabel[Label, Fields <: Tuple] <: Boolean =
  Fields match {
    case EmptyTuple              => false
    case Field[Label, _] *: tail => true
    case head *: tail            => HasLabel[Label, tail]
  }

summon[HasLabel["age", Fields] =:= true]

type LabelsUnion[Labels <: Tuple] =
  Labels match {
    case EmptyTuple          => Nothing
    case label *: EmptyTuple => label | label
    case label *: tail       => label | LabelsUnion[tail]

  }

summon[LabelsUnion[companyMirror.MirroredElemLabels] =:= ("age" | "name")]
summon[
  ("left" | "right" | "above") =:=
    LabelsUnion["left" *: "right" *: "above" *: EmptyTuple]
]

summon[Tuple.Union[companyMirror.MirroredElemLabels] =:= ("age" | "name")]
