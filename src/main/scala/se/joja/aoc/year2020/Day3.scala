package se.joja.aoc.year2020

import scala.annotation.tailrec
import scala.io.Source

object Day3 extends App {

  val input = Source.fromResource("2020/day3.txt").getLines().toSeq

  val width = input.head.length
  val height = input.size

  val data = input.map { line =>
    val trees = line.zipWithIndex.filter(_._1 == '#').map(_._2).toList
    println(s"Length: $width, $trees")
    trees
  }

  case class Pos(x: Int, y: Int, hits: Int = 0) {
    def checkHit: Pos = {
      if (y >= height) {
        println(s"Value $y is outside")
        this
      } else if (data(y).contains(x % 31)) {
        println(s"Hit on $this")
        this.copy(hits = hits + 1)
      } else {
        println(s"Miss on $this")
        this
      }
    }

    def move(dx: Int, dy: Int): Pos = this.copy(x = x + dx, y = y + dy)
  }

  @tailrec
  def run(pos: Pos, move: (Int, Int)): Pos = {
    if (pos.y >= height) pos else {
      run(pos.checkHit.move(move._1, move._2), move)
    }
  }

  val result = run(Pos(0, 0), 3 -> 1)

  println(s"Result: $result")

  // Part two
  val moves = List(1 -> 1, 3 -> 1, 5 -> 1, 7 -> 1, 1 -> 2)

  val results = moves.map { move =>
    run(Pos(0, 0), move)
  }

  println(s"Results: $results")
  println(s"Final: ${results.map(_.hits.toLong).product}")
}
