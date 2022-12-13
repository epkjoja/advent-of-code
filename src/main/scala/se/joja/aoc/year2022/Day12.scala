package se.joja.aoc.year2022

import se.joja.aoc.getInput
import se.joja.aoc.util.{Dijkstra, DirectedEdge, EdgeWeightedDigraph}

object Day12 extends App {

  val input = getInput("2022/day12.txt").map(_.map {
    case 'S' => 0
    case 'E' => 27
    case c   => c.toInt - 96
  })

  println(input.map(_.mkString(" ")).mkString("\n"))

  val maxX = input.head.size
  val maxY = input.length
  println(s"x: $maxX, y: $maxY")

  val vec = input.reduceLeft(_ ++ _).toVector
  println(vec.size)
  println(vec)

  val graph = vec.zipWithIndex.foldLeft(EdgeWeightedDigraph(maxX * maxY)) {
    case (acc, (height, i)) =>
      val neighbours = List(i - 1, i + 1, i - maxX, i + maxX).filter(i => i >= 0 && i < maxX * maxY)

      neighbours.foldLeft(acc) {
        case (acc2, i2) =>
          if (vec(i2) <= height + 1) acc2.addEdge(DirectedEdge(i, i2, 1)) else acc2
      }
  }
  //println(graph)

  val startPos = vec.zipWithIndex.filter(_._1 == 0).head._2
  val endPos   = vec.zipWithIndex.filter(_._1 == 27).head._2
  println(s"start: $startPos, end: $endPos")

  val res  = Dijkstra.run(graph, startPos)
  val dist = res.pathTo(endPos)

  val result1 = dist.size
  println(s"Result part 1: $result1")

  // Part 2

  //println(s"Result part 2: $result2")
}
