package io.mango.day04

import io.mango.common.Utils._
import cats.syntax.all._

object Part1 extends App {
  val kv        = "([a-z]{3}):(.*)$".r
  val mandatory = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val passports = readLinesSplit("day04", "^$".r)
    .map(_.flatMap(_.split(" ")).map { case kv(key, value) => key }.toSet)
  println(passports.count(mandatory.subsetOf(_)))
}

object Part2 extends App {
  def mandatoryChecks(kv: (String, String)): Boolean = kv match {
    case ("byr", r"19[2-9][0-9]|200[0-2]")                            => true
    case ("iyr", r"201[0-9]|2020")                                    => true
    case ("eyr", r"202[0-9]|2030")                                    => true
    case ("hgt", r"(?:1[5-8][0-9]|19[0-3])cm|(?:59|6[0-9]|7[0-6])in") => true
    case ("hcl", r"#[0-9a-f]{6}")                                     => true
    case ("ecl", r"amb|blu|brn|gry|grn|hzl|oth")                      => true
    case ("pid", r"[0-9]{9}")                                         => true
    case _                                                            => false
  }

  val kv        = "([a-z]{3}):(.*)$".r
  val passports = readLinesSplit("day04", "^$".r)
    .map(_.flatMap(_.split(" ")).map { case kv(key, value) => key -> value }.toSet)

  println(passports.count(_.count(mandatoryChecks) == 7))
}
