package se.joja.aoc.year2019

import se.joja.aoc.{getInput, invertMap}

import scala.annotation.tailrec

object Day6 extends App {
  val input = getInput("2019/day6.txt")

  val map = input
    .map { s =>
      val parts = s.split("""\)""")
      parts(0) -> parts(1)
    }
    .groupBy(_._1)
    .map {
      case (key, list) =>
        key -> list.map(_._2)
    }

  val imap = invertMap(map).map { case (key, list) => key -> list.head}

  def count(key: String, acc: Int = 0): Int = {
    if (key == "COM") acc else count(imap(key)) + 1
  }

  val res = imap.keys.toList.map { key =>
    val c = count(key)
    //println(s"Count $key -> $c")
    c
  }

  println(s"Result: ${res.sum}, $res")

  // Part two

  @tailrec
  def countMap(key: String, acc: List[String] = List.empty): List[String] = {
    if (key == "COM") acc else countMap(imap(key), acc :+ key)
  }

  val sanMap = countMap("SAN").zipWithIndex.reverse
  val youMap = countMap("YOU").zipWithIndex.reverse
  println(sanMap)
  println(youMap)

  0.to(sanMap.size).foreach { i =>
    if (sanMap(i)._1 != youMap(i)._1)
      println(s"San: ${sanMap(i)}, You: ${youMap(i)}")
  }
}
