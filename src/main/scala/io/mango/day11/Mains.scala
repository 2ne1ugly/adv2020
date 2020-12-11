package io.mango.day11

import io.mango.common.Utils._
import io.mango.day09.Common.getInvalidNumber
import cats.implicits._

import scala.annotation.tailrec

object Part1 extends App {
  type Layout = IndexedSeq[IndexedSeq[Char]]

  val initialLayout = readLines("day11").map(_.toIndexedSeq).toIndexedSeq
  @tailrec
  def rec(layout: Layout): Int = {
    val nextLayout = layout.map(_.zipWithIndex).zipWithIndex.map { case (row, y) =>
      row.map { case (tile, x) =>
        lazy val adjCount = Array.tabulate(3, 3) { case (x, y) => (x - 1, y - 1) }.flatten.count { case (dx, dy) =>
          layout.lift(y + dy).flatMap(_.lift(x + dx)).contains('#')
        }
        tile match {
          case '#' => if (adjCount <= 4) '#' else 'L'
          case 'L' => if (adjCount == 0) '#' else 'L'
          case '.' => '.'
        }
      }
    }
    if (nextLayout == layout)
      layout.flatten.count(_ == '#')
    else
      rec(nextLayout)
  }

  println(rec(initialLayout))
}

object Part2 extends App {
  type Layout = IndexedSeq[IndexedSeq[Char]]

  val initialLayout = readLines("day11").map(_.toIndexedSeq).toIndexedSeq
  @tailrec
  def rec(layout: Layout): Int = {
    val nextLayout = layout.map(_.zipWithIndex).zipWithIndex.map { case (row, y) =>
      row.map { case (tile, x) =>
        lazy val adjCount = Array.tabulate(3, 3) { case (x, y) => (x - 1, y - 1) }.flatten.count { case (dx, dy) =>
          @tailrec
          def rec2(ddx: Int, ddy: Int): Boolean =
            layout.lift(y + ddx).flatMap(_.lift(x + ddy)) match {
              case Some('#') => true
              case Some('.') => rec2(ddx + dx, ddy + dy)
              case _         => false
            }
          rec2(dx, dy)
        }
        tile match {
          case '#' => if (adjCount <= 5) '#' else 'L'
          case 'L' => if (adjCount == 0) '#' else 'L'
          case '.' => '.'
        }
      }
    }
    if (nextLayout == layout)
      layout.flatten.count(_ == '#')
    else
      rec(nextLayout)
  }

  println(rec(initialLayout))
}
