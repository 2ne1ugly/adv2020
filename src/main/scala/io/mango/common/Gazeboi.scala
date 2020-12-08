package io.mango.common

import scala.annotation.tailrec
import scala.util.matching.Regex
import cats.syntax.all._

trait Gazeboi {
  sealed trait Instruction {
    val arg: Int
  }

  object Instruction {
    case class Acc(arg: Int) extends Instruction
    case class Jmp(arg: Int) extends Instruction
    case class Nop(arg: Int) extends Instruction
  }

  case class State(ip: Int = 0, acc: Int = 0)

  import Instruction._

  type Program = IndexedSeq[Instruction]

  val instructionPattern: Regex = "([a-z]{3}) ([+\\-][\\d]+)".r

  def parseProgram(rawProgram: List[String]): Program =
    rawProgram.map { case instructionPattern(op, arg) =>
      op match {
        case "acc" => Acc(arg.toInt)
        case "jmp" => Jmp(arg.toInt)
        case "nop" => Nop(arg.toInt)
      }
    }.toIndexedSeq

  def runInstruction(instruction: Instruction, state: State): State = instruction match {
    case Acc(arg) => State(state.ip + 1, state.acc + arg)
    case Jmp(arg) => State(state.ip + arg, state.acc)
    case Nop(arg) => State(state.ip + 1, state.acc)
  }

  def debugInfiniteLoop(program: Program): Either[Int, Int] = {
    @tailrec
    def runLine(state: State, visited: Set[Int]): Either[Int, Int] =
      if (visited(state.ip))
        state.acc.asLeft
      else
        program.lift(state.ip) match {
          case Some(instr) => runLine(runInstruction(instr, state), visited + state.ip)
          case None        => state.acc.asRight
        }

    runLine(State(), Set.empty)
  }

  def runProgram(program: Program): Int = {
    @tailrec
    def runLine(state: State): Int = {
      println(state)
      program.lift(state.ip) match {
        case Some(instr) => runLine(runInstruction(instr, state))
        case None        => state.acc
      }
    }

    runLine(State())
  }
}
