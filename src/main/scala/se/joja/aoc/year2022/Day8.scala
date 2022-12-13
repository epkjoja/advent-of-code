package se.joja.aoc.year2022

import java.util.concurrent.atomic.AtomicInteger

import se.joja.aoc.getInput

object Day8 extends App {

  val counter = new AtomicInteger(0)

  val input = getInput("2022/day8.txt").map(_.map(x => Tree(x.toInt - 48)).toList)

  case class Tree(height: Int, id: Int = counter.addAndGet(1))

  def findVisible(line: List[Tree]): List[Tree] = {
    //println(s"Line: $line")
    line.foldLeft((-1, List.empty[Tree])) { case ((max, visible), tree) =>
      val res = if (tree.height > max) (tree.height, visible :+ tree) else (max, visible)
      //println(s" - max: $max, curr: $tree, count: $visible")
      res
    }._2
  }

  def forMatrix(m: List[List[Tree]]): List[Tree] = {
    m.flatMap(findVisible)
  }

  val fromLeft = forMatrix(input)
  val fromRight = forMatrix(input.map(_.reverse))
  val fromTop = forMatrix(input.transpose)
  val fromBottom = forMatrix(input.transpose.map(_.reverse))

  println(fromLeft)
  println(fromRight)
  println(fromTop)
  println(fromBottom)

  val all = List(fromLeft, fromRight, fromTop, fromBottom).flatten.distinctBy(_.id).sortBy(_.id)
  println(all)

  val result1 = all.size
  println(s"Result part 1: $result1")

  // Part 2

  case class Forest(trees: Map[Int, Tree]) {

  }

  //println(s"Result part 2: $result2")
}
