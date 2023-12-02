package se.joja.aoc.year2023

import se.joja.aoc.getInput

object Day2 extends App {

  val input = getInput("2023/day2.txt")

  val maxRed = 12
  val maxGreen = 13
  val maxBlue = 14

  case class GameSet(r: Int = 0, g: Int = 0, b: Int = 0) {
    def power: Long = 1L * r * g * b
  }
  case class Game(id: Int, games: List[GameSet])

  val SetPartRegEx = """ (\d+) (red|green|blue)""".r

  val games = input.map { line =>
    val prefixStr :: setsStr :: Nil = line.split(':').toList
    val sets = setsStr.split(';').foldLeft(List.empty[GameSet]) { case (acc, setStr) =>
      val set = setStr.split(',').foldLeft(GameSet()) { case (acc, setPartStr) =>
        setPartStr match {
          case SetPartRegEx(num, "red") => acc.copy(r = num.toInt)
          case SetPartRegEx(num, "green") => acc.copy(g = num.toInt)
          case SetPartRegEx(num, "blue") => acc.copy(b = num.toInt)
        }
      }
      acc :+ set
    }
    Game(prefixStr.filter(_.isDigit).toInt, sets)
  }

  val gameMaxes = games.map { g =>
    g.id -> GameSet(
      r = g.games.map(_.r).max,
      g = g.games.map(_.g).max,
      b = g.games.map(_.b).max
    )
  }

  val possible = gameMaxes.filter { case (_, set) => set.r <= maxRed && set.g <= maxGreen && set.b <= maxBlue }

  val result1 = possible.map(_._1).sum

  println(s"Result part 1: ${result1}")

  // Part 2

  val powers = gameMaxes.map(_._2.power)

  val result2 = powers.sum

  println(s"Result part 2: ${result2}")
}

