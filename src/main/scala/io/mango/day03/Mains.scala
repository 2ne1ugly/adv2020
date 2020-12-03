package io.mango.day03

import io.mango.common.Utils._
import cats.syntax.all._

object Part1 extends App {
  println(readLines("day03").zipWithIndex.drop(1).count { case (row, idx) =>
    row((idx * 3) % row.length) == '#'
  })
}

object Part2 extends App {
  val rows = readLines("day03").zipWithIndex.drop(1)
  println(
    List(2, 6, 10, 14, 1).map { slope =>
      rows.count { case (row, idx) =>
        (slope * idx) % 2 == 0 && row((slope.toInt * idx / 2) % row.length) == '#'
      }.toLong
    }.product
  )
}
