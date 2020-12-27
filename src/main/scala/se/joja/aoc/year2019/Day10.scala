package se.joja.aoc.year2019

import se.joja.joja.{gcd, getInput}

object Day10 extends App {
  val input = getInput("2019/day10_test.txt")

  case class Point(x: Int, y: Int)

  val allAst = input.zipWithIndex.flatMap {
    case (row, yi) =>
      row.zipWithIndex.flatMap {
        case ('#', xi) => Some(Point(xi, yi))
        case _         => None
      }
  }

  def check(to: Point, from: Point): List[Point] = {
    val (dx, dy) = (from.x - to.x, from.y - to.y)
    println(s"dX: $dx, dY: $dy -> ${gcd(dx, dy)}")
    List.empty
  }

  println(allAst)

  val j = allAst.sliding(2).toList.map {
    case p1 :: p2 :: Nil => check(p1, p2)
    case _               => println("JOJA")
  }

  println(j)
}
