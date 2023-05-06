// Scala literal types https://docs.scala-lang.org/sips/42.type.html

val x: 42 = 42

val y: x.type = 42

valueOf[x.type]
valueOf[y.type]

