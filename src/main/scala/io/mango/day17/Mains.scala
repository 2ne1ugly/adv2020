package io.mango.day17

import io.mango.common.Utils._
import cats.implicits._

object Part1 extends App {
  val charMap = readLines("day17").zipWithIndex.flatMap { case (row, y) => row.zipWithIndex.map { case (char, x) => (x, y, 0) -> char } }.toMap
  def updateAdj(pos: (Int, Int, Int), oldChar: Char, newChar: Char, adj: Map[(Int, Int, Int), Int]): Map[(Int, Int, Int), Int] = {
    if (oldChar == newChar)
      adj
    else
      Array
        .tabulate(3, 3, 3) { case (dx, dy, dz) => (dx - 1, dy - 1, dz - 1) }
        .flatten
        .flatten
        .foldLeft(adj) { case (adj, delta) =>
          adj.updatedWith(pos combine delta)(_.orElse(0.some).map(_ + (if (newChar == '#') 1 else -1)))
        }
  }

  val adj = charMap.foldLeft(Map.empty[(Int, Int, Int), Int]) { case (adj, (pos, char)) => updateAdj(pos, '.', char, adj) }
  val ret = (0 until 6).foldLeft((adj, charMap)) { case ((adj, charMap), _) =>
    adj.keys
      .map { pos =>
        val adjCount = adj.getOrElse(pos, 0)
        val char     = charMap.getOrElse(pos, '.')
        if ((char == '#' && (adjCount == 3 || adjCount == 4)) || (char == '.' && adjCount == 3))
          (pos, char, '#')
        else
          (pos, char, '.')
      }
      .foldLeft((adj, charMap)) { case ((adj, charMap), (pos, oldChar, newChar)) =>
        (updateAdj(pos, oldChar, newChar, adj), charMap.updated(pos, newChar))
      }
  }

  println(ret._2.values.count(_ == '#'))
}

object Part2 extends App {
  val charMap = readLines("day17").zipWithIndex.flatMap { case (row, y) => row.zipWithIndex.map { case (char, x) => (x, y, 0, 0) -> char } }.toMap
  def updateAdj(pos: (Int, Int, Int, Int), oldChar: Char, newChar: Char, adj: Map[(Int, Int, Int, Int), Int]): Map[(Int, Int, Int, Int), Int] = {
    if (oldChar == newChar)
      adj
    else
      Array
        .tabulate(3, 3, 3, 3) { case (dx, dy, dz, dw) => (dx - 1, dy - 1, dz - 1, dw - 1) }
        .flatten
        .flatten
        .flatten
        .foldLeft(adj) { case (adj, delta) =>
          adj.updatedWith(pos combine delta)(_.orElse(0.some).map(_ + (if (newChar == '#') 1 else -1)))
        }
  }

  val adj = charMap.foldLeft(Map.empty[(Int, Int, Int, Int), Int]) { case (adj, (pos, char)) => updateAdj(pos, '.', char, adj) }
  val ret = (0 until 6).foldLeft((adj, charMap)) { case ((adj, charMap), _) =>
    adj.keys
      .map { pos =>
        val adjCount = adj.getOrElse(pos, 0)
        val char     = charMap.getOrElse(pos, '.')
        if ((char == '#' && (adjCount == 3 || adjCount == 4)) || (char == '.' && adjCount == 3))
          (pos, char, '#')
        else
          (pos, char, '.')
      }
      .foldLeft((adj, charMap)) { case ((adj, charMap), (pos, oldChar, newChar)) =>
        (updateAdj(pos, oldChar, newChar, adj), charMap.updated(pos, newChar))
      }
  }

  println(ret._2.values.count(_ == '#'))
}
