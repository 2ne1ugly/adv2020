package io.mango.day10

import io.mango.common.Utils._
import io.mango.day09.Common.getInvalidNumber
import cats.implicits._

import scala.annotation.tailrec

object Part1 extends App {
  val adapters = readLines("day10").map(_.toInt).sorted
  val diffs    = adapters
    .prepended(0)
    .appended(adapters.last + 3)
    .sliding(2)
    .toList
    .map { case el1 :: el2 :: Nil =>
      el2 - el1 -> 1
    }
    .groupMapReduce(_._1)(_._2)(_ + _)
  println(diffs(1) * diffs(3))
}

object Part2 extends App {
  val adapters                  = readLines("day10").map(_.toInt).sorted
  val seq                       = adapters.prepended(0).appended(adapters.last + 3).toIndexedSeq
  lazy val count: Int => BigInt = memoize {
    case idx if idx == seq.size - 1 => 1
    case idx                        => seq.drop(idx + 1).takeWhile(_ <= seq(idx) + 3).zipWithIndex.map(_._2 + idx + 1).map(count).sum
  }
  println(count(0))
}
