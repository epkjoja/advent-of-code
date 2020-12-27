package se.joja.aoc.year2020

import se.joja.joja.getInput

object Day12 extends App {

  val input = getInput("2020/day12.txt").map(_.span(_.isLetter)).map(i => i._1 -> i._2.toInt)

  case class Point(x: Int, y: Int, facing: Int = 90) {

    def move(instr: (String, Int)): Point = {
      instr match {
        case ("N", v) => copy(y = y + v)
        case ("S", v) => copy(y = y - v)
        case ("E", v) => copy(x = x + v)
        case ("W", v) => copy(x = x - v)
        case ("L", v) => rotate(-v)
        case ("R", v) => rotate(v)
        case ("F", v) =>
          facing match {
            case 0   => copy(y = y + v)
            case 90  => copy(x = x + v)
            case 180 => copy(y = y - v)
            case 270 => copy(x = x - v)
          }
      }
    }

    def rotate(deg: Int): Point = {
      val temp      = (facing + deg) % 360
      val newFacing = if (temp < 0) temp + 360 else temp
      copy(facing = newFacing)
    }

    def manhattan: Int = x.abs + y.abs
  }

  val res = input.foldLeft(Point(0, 0)) {
    case (acc, move) =>
      acc.move(move)
  }

  println(s"Result: ${res.manhattan}")

  // Part two

  type Pnt = (Int, Int)

  case class System(ship: Pnt, waypnt: Pnt) {

    def move(instr: (String, Int)): System = {
      instr match {
        case ("N", v) => copy(waypnt = (waypnt._1, waypnt._2 + v))
        case ("S", v) => copy(waypnt = (waypnt._1, waypnt._2 - v))
        case ("E", v) => copy(waypnt = (waypnt._1 + v, waypnt._2))
        case ("W", v) => copy(waypnt = (waypnt._1 - v, waypnt._2))
        case ("L", v) => rotate(-v)
        case ("R", v) => rotate(v)
        case ("F", v) => copy(ship = (ship._1 + waypnt._1 * v, ship._2 + waypnt._2 * v))
      }
    }

    def rotate(deg: Int): System = {
      val normDeg = if (deg < 0) deg % 360 + 360 else deg % 360

      val newWp = normDeg match {
        case 0   => waypnt
        case 90  => (waypnt._2, -waypnt._1)
        case 180 => (-waypnt._1, -waypnt._2)
        case 270 => (-waypnt._2, waypnt._1)
      }
      copy(waypnt = newWp)
    }

    def manhattan: Int = ship._1.abs + ship._2.abs
  }

  val res2 = input.foldLeft(System((0, 0), (10, 1))) {
    case (acc, move) =>
      val newAcc = acc.move(move)
      println(s"Move: $move -> $newAcc")
      newAcc
  }

  println(s"Result: ${res2.manhattan}")
}
