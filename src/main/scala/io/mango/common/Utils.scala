package io.mango.common

import scala.io.Source

object Utils {
  def readLines(path: String): List[String] = {
    val source = Source.fromResource(path)
    try source.getLines().toList
    finally source.close()
  }
}
