// ==================== Tuple ====================
// head
summon[Tuple.Head[(Int, String, Double)] =:= Int]

// tail
summon[Tuple.Tail[(Int, String, Double)] =:= (String, Double)]
// res0: =:=[*:[String, *:[Double, EmptyTuple]], *:[String, *:[Double, EmptyTuple]]] = generalized constraint

// append
summon[Tuple.Append[(Int, String), Double] =:= (Int, String, Double)]

// init
summon[Tuple.Init[(Int, String, Double)] =:= (Int, String)]

// last
summon[Tuple.Last[(Int, String, Double)] =:= (Double)]

// concat
summon[
  Tuple.Concat[(Int, String, Double), (Int, String, Double)] =:=
    (Int, String, Double, Int, String, Double)
]
// res5: =:=[*:[Int, *:[String, *:[Double, Tuple3[Int, String, Double]]]], *:[Int, *:[String, *:[Double, Tuple3[Int, String, Double]]]]] = generalized constraint
summon[Tuple.Concat[EmptyTuple, (Int, Int)] =:= (Int, Int)]

// elem
summon[Tuple.Elem[(Int, String, Double), 0] =:= Int]
summon[Tuple.Elem[(Int, String, Double), 1] =:= String]
summon[Tuple.Elem[(Int, String, Double), 2] =:= Double]

// size
summon[Tuple.Size[(Int, String, Double)] =:= 3]

// map
summon[
  Tuple.Map[(Int, String, Double), Option] =:=
    (Option[Int], Option[String], Option[Double])
]
// res11: =:=[*:[Option[Int], *:[Option[String], *:[Option[Double], EmptyTuple]]], *:[Option[Int], *:[Option[String], *:[Option[Double], EmptyTuple]]]] = generalized constraint

// ==================== Int ====================
import scala.compiletime.ops.int._

summon[2 + 2 =:= 4]
summon[2 * 2 =:= 4]
summon[4 >> 2 =:= 1]
summon[2 << 2 =:= 8]

// ==================== String ====================
{
  import scala.compiletime.ops.string._

  summon["hello" + "world" =:= "helloworld"]
}

import scala.compiletime.ops.string._

summon[Length["hello"] =:= 5]

summon[CharAt["hello", 4] =:= 'o']

summon[Substring["hello", 0, 4] =:= "hell"]

summon[Matches["hello", "^hello$"] =:= true]
summon[Matches["hello", "^hell$"] =:= false]

// ==================== Any ====================
import scala.compiletime.ops.any._

summon[1 == 1 =:= true]
summon[1 == "1" =:= false]
summon[IsConst[3] =:= true]
summon[IsConst[Int] =:= false]
summon[IsConst[Any] =:= false]

{
  import scala.compiletime.ops.any._
  summon[ToString[3] =:= "3"]
  summon[ToString[true] =:= "true"]

}

// ==================== Other ====================

import scala.compiletime.ops.boolean
import scala.compiletime.ops.double
import scala.compiletime.ops.float
import scala.compiletime.ops.long
