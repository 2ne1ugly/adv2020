package io.mango.day07

import io.mango.common.Utils._
import cats.implicits._

import scala.annotation.tailrec

object Part1 extends App {
  val statement    = "([a-z]+ [a-z]+) bags contain(.*)\\.".r
  val subStatement = " (\\d+) ([a-z]+ [a-z]+) bags?".r

  val rules =
    readLines("day07").map { case statement(lhs, rhs) =>
      lhs -> rhs
        .split(',')
        .filter(_ != " no other bags")
        .map { case subStatement(quantity, color) => color -> quantity.toInt }
        .toMap
    }.toMap

  def rec(color: String): Boolean = {
    val bags = rules(color)
    if (bags.contains("shiny gold"))
      true
    else
      bags.keySet.exists(rec)
  }

  println(rules.keySet.count(rec))
}

object Part2 extends App {
  val statement    = "([a-z]+ [a-z]+) bags contain(.*)\\.".r
  val subStatement = " (\\d+) ([a-z]+ [a-z]+) bags?".r

  val rules =
    readLines("day07").map { case statement(lhs, rhs) =>
      lhs -> rhs
        .split(',')
        .filter(_ != " no other bags")
        .map { case subStatement(quantity, color) => color -> quantity.toInt }
        .toMap
    }.toMap

  def rec(color: String): Int = {
    rules(color).toList.foldMap { case (color, quantity) => quantity * rec(color) } + 1
  }

  println(rec("shiny gold") - 1)
}
