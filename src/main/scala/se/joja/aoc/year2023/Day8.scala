package se.joja.aoc.year2023

import se.joja.aoc
import se.joja.aoc.getInput

object Day8 extends App {

  val input = getInput("2023/day8.txt")
  val movesRaw :: _ :: nodesRaw = input

  case class Node(start: String, left: String, right: String) {

  }
  val NodeRegEx = """(\w+) = \((\w+), (\w+)\)""".r
  val nodesMap = nodesRaw.map {
    case NodeRegEx(start, endLeft, endRight) => start -> Node(start, endLeft, endRight)
  }.toMap

  val movesIter = Iterator.unfold(0) { i =>
    if (i >= movesRaw.length) Some(movesRaw.charAt(0), 1) else Some(movesRaw.charAt(i), i + 1)
  }

  var found = false
  val path = movesIter.takeWhile(_ => !found).foldLeft(List("AAA")) { case (acc, n) =>
    val currLoc = acc.last
    println(s"currLoc: $currLoc, n: $n")

    val nextLoc = n match {
        case 'L' => nodesMap(currLoc).left
        case 'R' => nodesMap(currLoc).right
    }

    if (nextLoc == "ZZZ") found = true
    acc :+ nextLoc
  }

  val result1 = path.length - 1

  println(s"Result part 1: ${result1}")

  // Part 2

  val starts = nodesMap.keys.filter(_.endsWith("A")).toList

  case class Pos(move: Char, idx: Int, node: String = "")

  def movesIter2 = Iterator.unfold(0) { i =>
    val newIdx = (i + 1) % movesRaw.length
    Some(Pos(movesRaw.charAt(i), i), newIdx)
  }

  def findPathLength(start: String): Long = {
    var found = false
    val iter = movesIter2
    val path = iter.takeWhile(_ => !found).foldLeft(List(Pos('-', -1, start))) { case (acc, n) =>
      val currLoc = acc.last.node
      println(s"currLoc: $currLoc, n: $n")

      val nextLoc = n.move match {
        case 'L' => nodesMap(currLoc).left
        case 'R' => nodesMap(currLoc).right
      }

      if (nextLoc.endsWith("Z")) found = true
      acc :+ n.copy(node = nextLoc)
    }
    path.length - 1
  }

  val result2 = aoc.lcm(starts.map(findPathLength))
  println(s"Result part 2: ${result2}")
  // List(23147, 20803, 17287, 19631, 15529, 17873)
}

