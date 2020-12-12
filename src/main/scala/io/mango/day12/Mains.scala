package io.mango.day12

import io.mango.common.Utils._
import io.mango.day09.Common.getInvalidNumber
import cats.implicits._

import scala.annotation.tailrec

sealed trait Direction {
  def left: Direction
}

object Direction {
  case object East extends Direction {
    def left: Direction = North
  }

  case object South extends Direction {
    def left: Direction = East
  }

  case object West extends Direction {
    def left: Direction = South
  }

  case object North extends Direction {
    def left: Direction = West
  }
}

object Part1 extends App {
  val instructions = readLines("day12").map(_.toList).map { case head :: tail => head -> tail.mkString.toInt }

  @tailrec
  def rec(pos: (Int, Int), dir: Direction, instructions: List[(Char, Int)]): Int = instructions match {
    case curr :: rest =>
      curr match {
        case ('N', arg)              => rec((pos._1, pos._2 + arg), dir, rest)
        case ('S', arg)              => rec((pos._1, pos._2 - arg), dir, rest)
        case ('E', arg)              => rec((pos._1 + arg, pos._2), dir, rest)
        case ('W', arg)              => rec((pos._1 - arg, pos._2), dir, rest)
        case ('L', 90) | ('R', 270)  => rec(pos, dir.left, rest)
        case ('L', 180) | ('R', 180) => rec(pos, dir.left.left, rest)
        case ('L', 270) | ('R', 90)  => rec(pos, dir.left.left.left, rest)
        case ('F', arg)              =>
          dir match {
            case Direction.North => rec((pos._1, pos._2 + arg), dir, rest)
            case Direction.South => rec((pos._1, pos._2 - arg), dir, rest)
            case Direction.East  => rec((pos._1 + arg, pos._2), dir, rest)
            case Direction.West  => rec((pos._1 - arg, pos._2), dir, rest)
          }
      }
    case Nil          => pos._1.abs + pos._2.abs
  }

  println(rec((0, 0), Direction.East, instructions))
}

object Part2 extends App {
  val instructions = readLines("day12").map(_.toList).map { case head :: tail => head -> tail.mkString.toInt }

  @tailrec
  def rec(pos: (Int, Int), wp: (Int, Int), instructions: List[(Char, Int)]): Int = instructions match {
    case curr :: rest =>
      curr match {
        case ('N', arg)              => rec(pos, (wp._1, wp._2 + arg), rest)
        case ('S', arg)              => rec(pos, (wp._1, wp._2 - arg), rest)
        case ('E', arg)              => rec(pos, (wp._1 + arg, wp._2), rest)
        case ('W', arg)              => rec(pos, (wp._1 - arg, wp._2), rest)
        case ('L', 90) | ('R', 270)  => rec(pos, (-wp._2, wp._1), rest)
        case ('L', 180) | ('R', 180) => rec(pos, (-wp._1, -wp._2), rest)
        case ('L', 270) | ('R', 90)  => rec(pos, (wp._2, -wp._1), rest)
        case ('F', arg)              => rec((pos._1 + wp._1 * arg, pos._2 + wp._2 * arg), wp, rest)
      }
    case Nil          => pos._1.abs + pos._2.abs
  }

  println(rec((0, 0), (10, 1), instructions))
}
