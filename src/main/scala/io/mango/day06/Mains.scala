package io.mango.day06

import io.mango.common.Utils._
import cats.implicits._
//import cats.syntax.all._

object Part1 extends App {
  println(
    readLinesSplit("day06", "^$".r)
      .foldMap(_.flatten.toSet.size)
  )
}

object Part2 extends App {
  println(
    readLinesSplit("day06", "^$".r)
      .foldMap(_.map(_.toSet).reduce(_ intersect _).size)
  )
}
