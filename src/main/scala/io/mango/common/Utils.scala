package io.mango.common

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.matching.Regex

object Utils {
  def readString(path: String): String = {
    val source = Source.fromResource(path)
    try source.mkString
    finally source.close()
  }

  def readLines(path: String): List[String] = {
    val source = Source.fromResource(path)
    try source.getLines().toList
    finally source.close()
  }

  def readLinesSplit(path: String, delimPattern: Regex): List[List[String]] = {
    @tailrec
    def splitBySeparator(lines: List[String], acc: List[List[String]]): List[List[String]] = {
      lines.span(!delimPattern.matches(_)) match {
        case (head, _ :: tail) => splitBySeparator(tail, acc.appended(head))
        case (head, _)         => acc ++ List(head)
      }
    }

    splitBySeparator(readLines(path), Nil)
  }

  implicit class RegexOps(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  implicit class stringOps(x: String) {
    def b: Long = BigInt.apply(x, 2).toLong
  }

  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I): O = getOrElseUpdate(key, f(key))
  }
}
