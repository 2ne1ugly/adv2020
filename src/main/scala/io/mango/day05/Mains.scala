package io.mango.day05

import io.mango.common.Utils._
import cats.syntax.all._

object Part1 extends App {
  println(
    readLines("day05")
      .map(_.replaceAll("[FL]", "0").replaceAll("[BR]", "1"))
      .map(Integer.parseInt(_, 2))
      .max
  )
}

object Part2 extends App {
  val ids = readLines("day05")
    .map(_.replaceAll("[FL]", "0").replaceAll("[BR]", "1"))
    .map(Integer.parseInt(_, 2))
  println(ids.fold(0)(_ ^ _) ^ (ids.min to ids.max).fold(0)(_ ^ _))
}
