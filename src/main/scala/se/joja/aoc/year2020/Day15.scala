package se.joja.aoc.year2020

import se.joja.aoc.getInput

object Day15 extends App {

  val input = getInput("2020/day15.txt")

//  val data = "0,3,6".split(',').toList.map(_.toLong)
  val data = input.head.split(',').toList.map(_.toLong)

  def calcNext(old: Map[Long, Int], last: Long, i: Int): (Map[Long, Int], Long) = {
    old.get(last) match {
      case None => (old + (last -> i), 0)
      case Some(pos) => (old + (last -> i), i - pos)
    }
  }

  val startMap = data.init.zipWithIndex.foldLeft(Map.empty[Long, Int]) { case (acc, (v, i)) => acc + (v -> i) }

//  val finding = 2020
  val finding = 30000000

  val (all, last) = ((data.size - 1) until finding - 1).foldLeft((startMap, data.last)) { case ((old, last), i) =>
//    println(s"Last: $last, i: $i, $old")
    calcNext(old, last, i)
  }

  println(s"Result: $last")
}
