package io.mango.day09

import io.mango.common.Utils._
import io.mango.day09.Common.getInvalidNumber

import scala.annotation.tailrec

object Common {
  def getInvalidNumber: Option[Long] = readLines("day09")
    .map(_.toLong)
    .reverse
    .sliding(26)
    .toList
    .findLast { case target :: tail =>
      val tailSet = tail.toSet
      !tail.exists(value => tailSet.contains(target - value))
    }
    .flatMap(_.headOption)
}

object Part1 extends App {
  println(getInvalidNumber)
}

object Part2 extends App {
  val invalidNumber = getInvalidNumber.get
  val seq           = readLines("day09").map(_.toLong).toIndexedSeq

  @tailrec
  def rec(lo: Int, hi: Int, acc: Long, occ: Map[Long, Int]): Long =
    if (acc == invalidNumber) {
      val keys = occ.filter(_._2 > 0).keySet
      keys.min + keys.max
    } else if (acc < invalidNumber)
      rec(lo, hi + 1, acc + seq(hi), occ + (seq(hi) -> 1))
    else
      rec(lo + 1, hi, acc - seq(lo), occ + (seq(lo) -> -1))

  println(rec(0, 0, 0, Map.empty))
}
