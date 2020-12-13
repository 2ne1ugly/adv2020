package io.mango.day13

import io.mango.common.Utils._
import io.mango.day09.Common.getInvalidNumber
import cats.implicits._

import scala.annotation.tailrec

object Part1 extends App {
  val (departTime, ids)    = readLines("day13") match { case departTime :: ids :: Nil => (departTime.toInt, ids.split(',').flatMap(_.toIntOption)) }
  val (id, minWaitingTime) = ids.map(id => (id, (id - (departTime % id)) % id)).minBy(_._2)
  println(id * minWaitingTime)
}

object Part2 extends App {
  // NOTE: https://en.wikipedia.org/wiki/Chinese_remainder_theorem
  // NOTE 2: https://www.geeksforgeeks.org/chinese-remainder-theorem-set-2-implementation/
  val ids     = readLines("day13")(1).split(',').zipWithIndex.flatMap(_.leftTraverse(_.toLongOption.map(BigInt.apply)))
  val product = ids.map(_._1).product
  val lhs     = ids.toList.foldMap { case (mod, rem) =>
    rem * (BigInt(1) until mod).find(product / mod % mod * _ % mod == 1).get * product / mod
  }
  println(product - lhs % product)
}
