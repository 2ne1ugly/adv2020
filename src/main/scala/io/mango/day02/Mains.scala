package io.mango.day02

import io.mango.common.Utils._
import cats.syntax.all._

import scala.annotation.tailrec

object Part1 extends App {
  val entryPattern = raw"(\d+)-(\d+) ([a-z]): ([a-z]+)".r
  println(readLines("day02").count { case entryPattern(lo, hi, char, password) =>
    password.count(_ == char.head) >= lo.toInt && password.count(_ == char.head) <= hi.toInt
  })
}

object Part2 extends App {
  val entryPattern = raw"(\d+)-(\d+) ([a-z]): ([a-z]+)".r
  println(readLines("day02").count { case entryPattern(lo, hi, char, password) =>
    password.regionMatches(lo.toInt - 1, char, 0, 1) ^ password.regionMatches(hi.toInt - 1, char, 0, 1)
  })
}
