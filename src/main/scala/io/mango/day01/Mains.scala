package io.mango.day01

import io.mango.common.Utils._
import cats.syntax.all._

import scala.annotation.tailrec

object Part1 extends App {
  val entries = readLines("day01").map(_.toLong).sorted.toIndexedSeq

  @tailrec
  def rec(left: Int, right: Int): (Long, Long) = {
    val sum = entries(left) + entries(right)
    if (sum < 2020)
      rec(left + 1, right)
    else if (sum > 2020)
      rec(left, right - 1)
    else
      (entries(left), entries(right))
  }

  val ret = rec(0, entries.size - 1)
  println(ret)
  println(ret._1 * ret._2)
}

object Part2 extends App {
  val entries = readLines("day01").map(_.toLong).sorted

  @tailrec
  def rec(seq: IndexedSeq[Long], left: Int, right: Int, offset: Long): Option[(Long, Long, Long)] = {
    if (left >= right)
      return None

    val sum = seq(left) + seq(right)
    if (sum < 2020 - offset)
      rec(seq, left + 1, right, offset)
    else if (sum > 2020 - offset)
      rec(seq, left, right - 1, offset)
    else
      (offset, seq(left), seq(right)).some
  }

  val triplet = entries.tails.toList
    .map {
      case head :: tails => rec(tails.toIndexedSeq, 0, tails.size - 1, head)
      case Nil           => None
    }
    .find(_.isDefined)
    .flatten
    .get

  println(triplet)
  println(triplet._1 * triplet._2 * triplet._3)
}
