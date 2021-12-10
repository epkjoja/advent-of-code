package se.joja.aoc.year2021

import se.joja.aoc.getInput

object Day7 extends App {

  val input = getInput("2021/day7.txt").head.split(',').map(_.toInt).toList
  println(input)

  // Part 1

  val (min, max) = (input.min, input.max)

  def calcFuel(targetPos: Int, positions: List[Int]): Long =
    positions.foldLeft(0) { case (acc, i) => acc + Math.abs(i - targetPos) }

  val fuelList = min.to(max).map { pos =>
    pos -> calcFuel(pos, input)
  }
  println(fuelList.mkString("\n"))

  val minPos = fuelList.minBy(_._2)

  println(s"Result part 1: $minPos")

  // Part 2

  def calcFuel2(targetPos: Int, positions: List[Int]): Long =
    positions.foldLeft(0) {
      case (acc, i) =>
        val n = Math.abs(i - targetPos)
        acc + (n * (n + 1) / 2)
    }

  val fuelList2 = min.to(max).map { pos =>
    pos -> calcFuel2(pos, input)
  }

  val minPos2 = fuelList2.minBy(_._2)

  println(s"Result part 2: $minPos2")
}
