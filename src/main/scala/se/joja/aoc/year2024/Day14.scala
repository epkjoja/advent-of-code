package se.joja.aoc.year2024

import se.joja.aoc.readInput
import se.joja.aoc.util.{Area, Point}

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Day14 extends App {

  val Pattern = """p=(\d+),(\d+) v=([-\d]+),([-\d]+)""".r
//  val (xSize, ySize) = (11, 7)
  val (xSize, ySize) = (101, 103)

  case class Robot(pos: Point, dx: Int, dy: Int) {
    def move: Robot = this.copy(pos = Point((pos.x + xSize + dx) % xSize, (pos.y + ySize + dy) % ySize))
    def quadrant: Option[Int] = {
      val qx = if (pos.x < xSize / 2) Some(0) else if (pos.x > xSize / 2) Some(1) else None
      val qy = if (pos.y < ySize / 2) Some(0) else if (pos.y > ySize / 2) Some(1) else None
      for {
        x <- qx
        y <- qy
      } yield x * 2 + y
    }
  }

  val input = readInput(2024, 14).map {
    case Pattern(x, y, dx, dy) => Robot(Point(x.toInt, y.toInt), dx.toInt, dy.toInt)
  }

  def printRobots(r: List[Robot]): String = {
    val p = r.map(_.pos).groupBy(identity).toList.flatMap {
      case (_, l) => l.map(p => p.swap -> s"${l.size}")
    }
    "\n" + Area(p, ".").toString
  }

  @tailrec
  def moveRobots(r: List[Robot], rounds: Int): List[Robot] =
    if (rounds == 0) r else moveRobots(r.map(_.move), rounds - 1)

//  val test = List(Robot(Point(2, 4), 2, -3))
  val moved = moveRobots(input, 100)
  val inQuads = moved.groupMapReduce(_.quadrant)(_ => 1L)(_ + _).toList.filter(_._1.isDefined)

  val result1 = inQuads.map(_._2).product

  println(s"Result part 1: $result1")

  // Part 2

  @tailrec
  def moveRobots2(r: List[Robot], round: Int = 1): List[Robot] = {
    val step = r.map(_.move)

    val inQuads = step.groupMapReduce(_.quadrant)(_ => 1L)(_ + _).filter(_._1.isDefined).map(r => r._1.get -> r._2)
    val quadCond = (inQuads(0) - inQuads(2)).abs < 2 && (inQuads(1) - inQuads(3)).abs < 2 &&
      inQuads(0) < inQuads(1) && inQuads(2) < inQuads(3)

    val cond = step.count(_.pos.y == ySize / 2 + 1) > xSize - 10
    if (quadCond) {
      println(s"${printRobots(step)} \nStep: $round")
      readLine()
    }

    moveRobots2(step, round + 1)
  }

  val result2 = moveRobots2(input)

  println(s"Result part 2: $result2")
}

