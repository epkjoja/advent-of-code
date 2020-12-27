package se.joja.aoc.year2020

import se.joja.joja.getInput

object Day24 extends App {

  val input = getInput("2020/day24.txt")

  case class Tile(x: Int = 0, y: Int = 0) {

    def move(moves: List[String]): Tile =
      moves match {
        case Nil       => this
        case m :: rest => moveOne(m).move(rest)
      }

    def moveOne(move: String): Tile =
      move match {
        case "NE" => this.copy(y = y + 1)
        case "E"  => this.copy(x = x + 1)
        case "SE" => this.copy(x = x + 1, y = y - 1)
        case "SW" => this.copy(y = y - 1)
        case "W"  => this.copy(x = x - 1)
        case "NW" => this.copy(x = x - 1, y = y + 1)
      }

    def neighbours: Set[Tile] =
      Set("NE", "E", "SE", "SW", "W", "NW").map(moveOne)
  }

  def parseMove(in: List[Char]): List[String] =
    in match {
      case Nil                => List.empty
      case 'n' :: 'e' :: rest => List("NE") ++ parseMove(rest)
      case 'e' :: rest        => List("E") ++ parseMove(rest)
      case 's' :: 'e' :: rest => List("SE") ++ parseMove(rest)
      case 's' :: 'w' :: rest => List("SW") ++ parseMove(rest)
      case 'w' :: rest        => List("W") ++ parseMove(rest)
      case 'n' :: 'w' :: rest => List("NW") ++ parseMove(rest)
    }

  val allMoves = input.map(line => parseMove(line.toList))

  val floor = allMoves.foldLeft(Set.empty[Tile]) { (acc, m) =>
    val endTile = Tile().move(m)
    if (acc.contains(endTile)) acc - endTile else acc + endTile
  }

  //println(blackTiles.mkString("\n"))
  println(s"Result: ${floor.size}")

  // Part two

  def countBlackNeigbours(black: Set[Tile], tile: Tile): Int =
    tile.neighbours.count(black)

  def oneDay(blacks: Set[Tile]): Set[Tile] = {
    val toWhite = blacks.filterNot(t => 1.to(2).contains(countBlackNeigbours(blacks, t)))
    val allWhiteNeigboursToBlacks = blacks.flatMap(_.neighbours).diff(blacks)
    val toBlack = allWhiteNeigboursToBlacks.filter(t => countBlackNeigbours(blacks, t) == 2)

    blacks -- toWhite ++ toBlack
  }

  def printDay(list: Set[Tile], numDays: Int, day: Int = 0): Unit = {
    println(s"Day $day: ${list.size}")
    if (day < numDays) printDay(oneDay(list), numDays, day + 1)
  }

  printDay(floor, 100)
  //println(printDay(Set(Tile(), Tile(0,1), Tile(1,0)), 2))
}
