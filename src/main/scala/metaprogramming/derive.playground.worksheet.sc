import io.circe.Derivation
import scala.deriving._
import scala.compiletime.{constValue, erasedValue, summonInline}

// ==================== safe show,
// by example from https://www.youtube.com/watch?v=leIB5tvDY64&ab_channel=Ziverge
// could be done nicer based on https://www.youtube.com/watch?v=X2EdbCO5e90&ab_channel=Ziverge,
//      but there are still `isInsanceOf`

trait SafeShow[T] {
  def show(t: T): String
}

object SafeShow {

  import scala.compiletime.{erasedValue, constValue, summonAll, constValueTuple}

  given safeShowAny[T]: SafeShow[T] with {
    def show(t: T): String = t.toString
  }

  inline def derived[T](using p: Mirror.ProductOf[T]): SafeShow[T] =
    val classLabel = constValue[p.MirroredLabel]
    val labels = labelsToList[p.MirroredElemLabels]
    // val shows = summonShowAll[p.MirroredElemTypes].toList.asInstanceOf[List[SafeShow[Any]]]
    // or
    val shows = summonAll[Tuple.Map[p.MirroredElemTypes, SafeShow]].toList
      .asInstanceOf[List[SafeShow[Any]]]
    new SafeShow[T]:
      def show(t: T): String = {
        val values = t.asInstanceOf[Product].productIterator.toList
        val fieldString = labels
          .zip(values)
          .zip(shows)
          .map { case ((l, v), show) =>
            val shown = show.show(v)
            l + "=" + shown
          }
          .mkString(",")
        s"$classLabel($fieldString)"
      }

  private inline def summonShowAll[T <: Tuple]: List[SafeShow[_]] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => summonInline[SafeShow[t]] :: summonShowAll[ts]
    }

  private inline def labelsToList[T <: Tuple]: List[String] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => constValue[t].toString :: labelsToList[ts]
    }

}

extension [T](t: T) {
  def safeShow(using s: SafeShow[T]): String = s.show(t)
}

case class Test1(f1: Int, f2: String) derives SafeShow

Test1.unapply(Test1(1, "2"))

println(Test1(1, "2").safeShow)

// ====================

// ====================
// based on https://www.youtube.com/watch?v=X2EdbCO5e90&ab_channel=Ziverge

import scala.compiletime.{erasedValue, constValue, summonAll, constValueTuple}

trait Show[T] {
  def show(t: T): String
}

trait Param[A, TypeClass[_]] {
  type PType
  def label: String
  def typeclass: TypeClass[PType]
  def deref(a: A): PType
}

trait CaseClass[A, TypeClass[_]] {
  def label: String
  def params: List[Param[A, TypeClass]]
}

trait Derivation[TypeClass[_]] {

  def join[A](caseClass: CaseClass[A, TypeClass]): TypeClass[A]

  inline def derived[A](using m: Mirror.ProductOf[A]): TypeClass[A] =
    val classLabel: String = constValue[m.MirroredLabel]
    val labels: List[String] =
      constValueTuple[m.MirroredElemLabels].toList.asInstanceOf[List[String]]
    val typeclasses: List[TypeClass[Any]] =
      summonAll[Tuple.Map[m.MirroredElemTypes, TypeClass]].toList
        .asInstanceOf[List[TypeClass[Any]]]

    val classParams: List[Param[A, TypeClass]] =
      labels.zip(typeclasses).zipWithIndex.map { case ((l, ts), idx) =>
        new Param[A, TypeClass] {
          def label: String = l
          def typeclass: TypeClass[PType] = ts.asInstanceOf[TypeClass[PType]]
          def deref(a: A): PType =
            a.asInstanceOf[Product].productElement(idx).asInstanceOf[PType]
        }
      }

    val caseClass = new CaseClass[A, TypeClass] {
      def label: String = classLabel
      def params: List[Param[A, TypeClass]] = classParams
    }

    join[A](caseClass)
}

object Show extends Derivation[Show] {

  given showAny[T]: Show[T] with {
    def show(t: T): String = t.toString
  }
  override def join[A](caseClass: CaseClass[A, Show]): Show[A] =
    new Show[A] {
      def show(a: A): String = {
        val classLabel = caseClass.label
        val classParams = caseClass.params
          .map { param =>
            val label = param.label
            val show = param.typeclass
            val value = param.deref(a)
            val shownValue = show.show(value)
            s"$label = $shownValue"
          }
          .mkString(", ")
        s"$classLabel($classParams)"
      }
    }
}

extension [T](t: T) {
  def show(using s: Show[T]): String = s.show(t)
}

case class Test2(f1: Int, f2: String) derives Show

summon[Show[Test2]].show(Test2(1, "2"))
