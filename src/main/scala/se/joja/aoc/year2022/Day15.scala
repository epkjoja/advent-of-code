package se.joja.aoc.year2022

import se.joja.aoc.getInput

object Day15 extends App {

  val input = getInput("2022/day15.txt")

  case class P(x: Int, y: Int) {
    def dist(that: P): Int =
      (x - that.x).abs + (y - that.y).abs

    def inSearchArea(max: Long): Boolean =
      x >= 0 && x <= max && y >= 0 && y <= max

    def frequency(max: Long): Long =
      x * max + y
  }

  case class Sensor(at: P, closest: P) {
    val dist: Int = at.dist(closest)

    def hitsOnLine(y: Int, max: Option[Int] = None): Set[Int] = {
      val diff = dist - (at.y - y).abs
      if (diff < 0) Set.empty
      else if (max.isEmpty) (at.x - diff).to(at.x + diff).toSet
      else {
        val minX = (at.x - diff).max(0)
        val maxX = (at.x + diff).min(max.get)
        minX.to(maxX).toSet
      }
    }

    def borderingToDist(max: Long): List[P] =
      0.to(dist)
        .flatMap { i =>
          List(
            P(at.x + i, at.y - dist - 1 + i),
            P(at.x + dist + 1 - i, at.y + i),
            P(at.x - i, at.y + dist + 1 - i),
            P(at.x - dist - 1 + i, at.y - i)
          ).filter(_.inSearchArea(max))
        }
        .toList

    def isWithinDist(p: P): Boolean =
      at.dist(p) <= dist
  }

  val InSensor = """Sensor at x=([-\d]+), y=([-\d]+): closest beacon is at x=([-\d]+), y=([-\d]+)""".r

  val sensors = input.map {
    case InSensor(sx, sy, bx, by) => Sensor(P(sx.toInt, sy.toInt), P(bx.toInt, by.toInt))
  }

  val lineNum       = 2000000
  val hitsOnLine    = sensors.flatMap(_.hitsOnLine(lineNum)).toSet
  val beaconsOnLine = sensors.filter(_.closest.y == lineNum).map(_.closest.x).toSet

  println(hitsOnLine.size)
  println(beaconsOnLine)

  val result1 = hitsOnLine.size - beaconsOnLine.size

  println(s"Result part 1: $result1")

  // Part 2

  val max = 4000000L

  val possibleBeacons = sensors.flatMap(_.borderingToDist(max)).distinct

  println(possibleBeacons.size)

  val found = possibleBeacons.filter(pb => !sensors.exists(_.isWithinDist(pb)))

  println(found)

  val result2 = found.map(_.frequency(max))

  println(s"Result part 2: $result2")
}
