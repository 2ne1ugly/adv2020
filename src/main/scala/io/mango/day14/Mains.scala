package io.mango.day14

import io.mango.common.Utils._
import io.mango.day09.Common.getInvalidNumber
import cats.implicits._

import scala.annotation.tailrec

object Part1 extends App {
  val maskPattern = "mask = (.*)".r
  val memPattern  = "mem\\[(\\d*)] = (\\d*)".r
  val program     = readLines("day14")

  @tailrec
  def rec(mask: Long, delta: Long, ret: Map[Int, Long], program: List[String]): Map[Int, Long] = program match {
    case maskPattern(maskValue) :: rest =>
      rec(
        maskValue.map(char => if (char == 'X') '1' else '0').b,
        maskValue.map(char => if (char == '1') '1' else '0').b,
        ret,
        rest
      )

    case memPattern(address, value) :: rest =>
      rec(mask, delta, ret + (address.toInt -> (value.toInt & mask | delta)), rest)

    case Nil                                => ret
  }

  println(rec(0, 0, Map.empty, program).values.sum)
}

object Part2 extends App {
  val maskPattern = "mask = (.*)".r
  val memPattern  = "mem\\[(\\d*)] = (\\d*)".r
  val program     = readLines("day14")

  @tailrec
  def rec(mask: Long, delta: Long, floatings: List[Long], ret: Map[Long, Long], program: List[String]): Map[Long, Long] = program match {
    case maskPattern(maskValue) :: rest =>
      rec(
        maskValue.map(char => if (char == 'X') '0' else '1').b,
        maskValue.replace('X', '0').b,
        maskValue.reverse.zipWithIndex
          .filter(_._1 == 'X')
          .map(_._2)
          .toSet
          .subsets
          .map(_.toList.foldMap(1L << _))
          .toList,
        ret,
        rest
      )

    case memPattern(address, value) :: rest =>
      rec(
        mask,
        delta,
        floatings,
        ret ++ floatings.map { offset =>
          ((address.toInt & mask | delta) + offset) -> value.toInt
        },
        rest
      )

    case Nil => ret
  }

  println(rec(0, 0, List.empty, Map.empty, program).values.sum)
}
