package se.joja.aoc.year2021

import se.joja.aoc.getInput

object Day6 extends App {

  val input = getInput("2021/day6.txt").head.split(',').map(_.toInt).toList
  println(input)

  // Part 1

  case class Fishes(fishCount: Map[Int, Long]) {
    def step: Fishes =
      Fishes(
        fishCount.toList
          .flatMap {
            case (0, count) => List(6     -> count, 8 -> count)
            case (t, count) => List(t - 1 -> count)
          }
          .groupBy(_._1)
          .map(e => e._1 -> e._2.map(_._2).sum)
      )

    def count: Long = fishCount.values.sum
  }
  object Fishes {
    def apply(fish: List[Int]): Fishes =
      Fishes(fish.groupBy(identity).map(e => e._1 -> e._2.size.toLong))
  }

  val fishes = Fishes(input)
  println(s"Fishes: $fishes")

  val finalFishes = 1.to(80).foldLeft(fishes) {
    case (acc, dayNum) =>
      val after = acc.step
      println(s"After day $dayNum: $after")
      after
  }

  val result1 = finalFishes.count
  println(s"Result part 1: $result1")

  // Part 2

  val finalFishes2 = 1.to(256).foldLeft(fishes) {
    case (acc, dayNum) =>
      val after = acc.step
      println(s"After day $dayNum: $after")
      after
  }

  val result2 = finalFishes2.count

  println(s"Result part 2: $result2")

}
