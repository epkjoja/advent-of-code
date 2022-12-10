package se.joja.aoc.year2022

import se.joja.aoc.getInput

object Day6 extends App {

  val input = getInput("2022/day6.txt")

  val result1 = input.map { line =>
    println(s"Testing line: $line")
    line.zipWithIndex.foldLeft((List.empty[Char], Option.empty[Int])) { case ((acc, res), (char, i)) =>
      if (res.isDefined) (acc, res) else {
        val acc2 = (acc :+ char).takeRight(4)
        val res2 = acc2 match {
          case a if a.length == 4 && a.toSet.size == 4 =>
            println(s"It's $i, c: $char, acc: $a")
            Some(i + 1)
          case _ =>
            None
        }
        (acc2, res2)
      }
    }
  }.map(x => x._1.mkString -> x._2 )

  println(s"Result part 1: $result1")

  // Part 2

  val result2 = input.map { line =>
    println(s"Testing line: $line")
    line.zipWithIndex.foldLeft((List.empty[Char], Option.empty[Int])) { case ((acc, res), (char, i)) =>
      if (res.isDefined) (acc, res) else {
        val acc2 = (acc :+ char).takeRight(14)
        val res2 = acc2 match {
          case a if a.length == 14 && a.toSet.size == 14 =>
            println(s"It's $i, c: $char, acc: $a")
            Some(i + 1)
          case _ =>
            None
        }
        (acc2, res2)
      }
    }
  }.map(x => x._1.mkString -> x._2)

  println(s"Result part 2: $result2")
}
