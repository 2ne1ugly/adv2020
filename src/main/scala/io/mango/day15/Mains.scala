package io.mango.day15

import io.mango.common.Utils._
import io.mango.day09.Common.getInvalidNumber
import cats.implicits._
import jdk.internal.jline.console.history.History

import scala.annotation.tailrec

object Part1 extends App {
  val input = readString("day15").split(",").map(_.toInt).toList

  @tailrec
  def rec(last: Int, year: Int, history: Map[Int, Int]): Int = {
    val current = history.get(last).map(year - _ - 1).getOrElse(0)
    if (year + 1 == 2020)
      current
    else
      rec(current, year + 1, history + (last -> (year - 1)))
  }

  println(rec(input.last, input.size, input.dropRight(1).zipWithIndex.toMap))
}

object Part2 extends App {
  // Same solution works, it just takes longer :)
  val input = readString("day15").split(",").map(_.toInt).toList

  @tailrec
  def rec(last: Int, year: Int, history: Map[Int, Int]): Int = {
    val current = history.get(last).map(year - _ - 1).getOrElse(0)
    if (year + 1 == 30000000)
      current
    else
      rec(current, year + 1, history + (last -> (year - 1)))
  }

  println(rec(input.last, input.size, input.dropRight(1).zipWithIndex.toMap))
}
