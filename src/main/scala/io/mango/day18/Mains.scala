package io.mango.day18

import io.mango.common.Utils._
import cats.implicits._

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait Operand

case class Single(number: BigInt)                                         extends Operand
case class Expression(operands: List[Operand], operators: List[Operator]) extends Operand

sealed trait Operator

case object Plus extends Operator
case object Mult extends Operator

object Part1 extends App {
  @tailrec
  def balance(string: List[Char], count: Int, acc: List[Char]): (List[Char], List[Char]) =
    string match {
      case '(' :: rest              => balance(rest, count + 1, acc appended '(')
      case ')' :: rest if count > 1 => balance(rest, count - 1, acc appended ')')
      case ')' :: rest              => (acc, rest)
      case other :: rest            => balance(rest, count, acc appended other)
    }

  def parse(chars: List[Char], operands: List[Operand], operators: List[Operator]): Expression = chars match {
    case value :: rest if value.isDigit => parse(rest, operands appended Single(BigInt(value - '0')), operators)
    case '(' :: rest                    =>
      val balanced = balance(rest, 1, Nil)
      parse(balanced._2, operands appended parse(balanced._1, Nil, Nil), operators)
    case '+' :: rest                    => parse(rest, operands, operators appended Plus)
    case '*' :: rest                    => parse(rest, operands, operators appended Mult)
    case ' ' :: rest                    => parse(rest, operands, operators)
    case Nil                            => Expression(operands, operators)
  }

  def evaluate(operand: Operand): BigInt = operand match {
    case Single(value)                   => value
    case Expression(operands, operators) =>
      (operands zip (Plus :: operators)).foldLeft(BigInt(0)) {
        case (ret, (operand, Plus)) => ret + evaluate(operand)
        case (ret, (operand, Mult)) => ret * evaluate(operand)
      }
  }

  println(
    readLines("day18")
      .map(_.toList)
      .map(parse(_, Nil, Nil))
      .map(evaluate)
      .sum
  )
}

object Part2 extends App {
  @tailrec
  def balance(string: List[Char], count: Int, acc: List[Char]): (List[Char], List[Char]) =
    string match {
      case '(' :: rest              => balance(rest, count + 1, acc appended '(')
      case ')' :: rest if count > 1 => balance(rest, count - 1, acc appended ')')
      case ')' :: rest              => (acc, rest)
      case other :: rest            => balance(rest, count, acc appended other)
    }

  def parse(chars: List[Char], operands: List[Operand], operators: List[Operator]): Expression = chars match {
    case value :: rest if value.isDigit => parse(rest, operands appended Single(BigInt(value - '0')), operators)
    case '(' :: rest                    =>
      val balanced = balance(rest, 1, Nil)
      parse(balanced._2, operands appended parse(balanced._1, Nil, Nil), operators)
    case '+' :: rest                    => parse(rest, operands, operators appended Plus)
    case '*' :: rest                    => parse(rest, operands, operators appended Mult)
    case ' ' :: rest                    => parse(rest, operands, operators)
    case Nil                            => Expression(operands, operators)
  }

  def evaluate(operand: Operand): BigInt = operand match {
    case Single(value)                   => value
    case Expression(operands, operators) =>
      def evaluateExpression(opts: List[(Operand, Operator)], acc: BigInt): BigInt = opts match {
        case (operand, Plus) :: rest => evaluateExpression(rest, acc + evaluate(operand))
        case (operand, Mult) :: rest => acc * evaluateExpression(rest, evaluate(operand))
        case Nil                     => acc
      }
      evaluateExpression(operands zip Plus :: operators, 0)

  }

  println(
    readLines("day18")
      .map(_.toList)
      .map(parse(_, Nil, Nil))
      .map(evaluate)
      .sum
  )

}
