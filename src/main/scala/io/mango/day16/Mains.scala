package io.mango.day16

import io.mango.common.Utils._
import cats.implicits._

object Part1 extends App {
  val rulePattern     = ".*: (\\d*)-(\\d*) or (\\d*)-(\\d*)".r
  val (rules, fields) = readLinesSplit("day16", "^$".r) match {
    case rules :: _ :: nearbyTickets :: Nil =>
      (
        rules.flatMap { case rulePattern(lo1, hi1, lo2, hi2) =>
          List((lo1.toInt, hi1.toInt), (lo2.toInt, hi2.toInt))
        },
        nearbyTickets.drop(1).flatMap(_.split(',')).map(_.toInt)
      )
  }

  println(fields.filterNot(field => rules.exists { case (lo, hi) => field >= lo && field <= hi }).size)
}

object Part2 extends App {
  val rulePattern                      = "(.*): (\\d*)-(\\d*) or (\\d*)-(\\d*)".r
  val (rules, myTicket, nearbyTickets) = readLinesSplit("day16", "^$".r) match {
    case rules :: myTicket :: nearbyTickets :: Nil =>
      (
        rules.map { case rulePattern(name, lo1, hi1, lo2, hi2) =>
          name -> List((lo1.toInt, hi1.toInt), (lo2.toInt, hi2.toInt))
        }.toMap,
        myTicket.drop(1).map(_.split(',').map(_.toInt)).head,
        nearbyTickets.drop(1).map(_.split(',').map(_.toInt))
      )
  }

  val filteredNearbyTickets =
    nearbyTickets.filter(_.forall(field => rules.values.flatten.exists { case (lo, hi) => field >= lo && field <= hi }))
  val rulesPerField         = (0 until rules.size)
    .map(idx =>
      idx -> rules.filter { case (_, rule) =>
        filteredNearbyTickets.forall { fields =>
          val field = fields(idx)
          rule.exists { case (lo, hi) => field >= lo && field <= hi }
        }
      }.keySet
    )
    .toMap

  lazy val rec: ((Set[Int], Set[String])) => Option[Map[String, Int]] =
    memoize[(Set[Int], Set[String]), Option[Map[String, Int]]] { case (restIdx, restRuleNames) =>
      if (restIdx.isEmpty || restRuleNames.isEmpty)
        Some(Map.empty)
      else
        restIdx.toList
          .sortBy(rulesPerField(_).size)
          .flatMap { idx =>
            restRuleNames
              .intersect(rulesPerField(idx))
              .toList
              .sorted
              .flatMap { ruleName =>
                rec(restIdx - idx, restRuleNames - ruleName).map(_ + (ruleName -> idx))
              }
          }
          .headOption
    }

  println(
    rec((0 until rules.size).toSet, rules.keySet).get
      .filter(_._1.startsWith("departure"))
      .values
      .map(myTicket(_).toLong)
      .product
  )
}
