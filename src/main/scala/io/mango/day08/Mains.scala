package io.mango.day08

import io.mango.common.Utils._
import cats.implicits._
import io.mango.common.Gazeboi
import scala.annotation.tailrec

object Part1 extends App {
  val gazeboi = new Gazeboi {}
  val program = gazeboi.parseProgram(readLines("day08"))
  println(gazeboi.debugInfiniteLoop(program))
}

object Part2 extends App {
  val gazeboi = new Gazeboi {}
  import gazeboi.Instruction._

  val program = gazeboi.parseProgram(readLines("day08"))
  println(program.zipWithIndex.flatMap { case (instruction, idx) =>
    instruction match {
      case Jmp(arg) => gazeboi.debugInfiniteLoop(program.updated(idx, Nop(arg))).toOption
      case Nop(arg) => gazeboi.debugInfiniteLoop(program.updated(idx, Jmp(arg))).toOption
      case _        => None
    }
  }.head)
}
