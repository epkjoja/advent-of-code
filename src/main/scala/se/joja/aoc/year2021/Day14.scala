package se.joja.aoc.year2021

import se.joja.aoc.{getInput, splitOnEmptyLine}

object Day14 extends App {

  val input                        = getInput("2021/day14.txt")
  val templateRaw :: rulesRaw :: _ = splitOnEmptyLine(input)
  val template                     = templateRaw.head.toList.map(_.toString)
  val rules                        = rulesRaw.map(s => s.take(2) -> s.takeRight(1)).toMap
  println(template)
  println(rules)

  // Part 1

  def step(in: List[String]): List[String] =
    in.sliding(2).foldLeft(List(in.head)) {
      case (acc, list) =>
        rules.get(list.mkString) match {
          case Some(insert) => acc :+ insert :+ list(1)
          case None         => acc :+ list(1)
        }
    }

  val finalTemplate = 1.to(10).foldLeft(template) {
    case (acc, i) =>
      val res = step(acc)
      println(s"After step $i: (${res.size}) ${res.mkString}")
      res
  }

  val countMap = finalTemplate.groupMapReduce(identity)(_ => 1)(_ + _)
  println(countMap)

  val result1 = countMap.maxBy(_._2)._2 - countMap.minBy(_._2)._2

  println(s"Result part 1: $result1")

  // Part 2

  def recurse(in: List[String], depth: Int): List[String] = {
    val out = if (depth == 0) in
    else {
      in.sliding(2).toList.flatMap { pair =>
        rules.get(pair.mkString) match {
          case Some(value) =>
            val a :: b :: Nil = pair
            recurse(List(a, value, b), depth - 1)
          case None => pair
        }
      }
    }
    println(s"Recurse: ($depth) - ${out.mkString}")
    out
  }

  val finalTemplate2 = recurse(template, 2)

  val countMap2 = finalTemplate2.groupMapReduce(identity)(_ => 1)(_ + _)
  println(countMap2)

  val result2 = countMap2.maxBy(_._2)._2 - countMap2.minBy(_._2)._2

  println(s"Result part 2: $result2")

}
