package se.joja.aoc.year2023

import se.joja.aoc.readInput
import se.joja.aoc.util.Point

import scala.annotation.tailrec

object Day16 extends App {

  val input = readInput(2023, 16)

  type Mat2D = Vector[Vector[Tile]]
  case class Tile(tpe: Char, p: Point, energised: Set[Char] = Set.empty)
  case class Beam(p: Point, dir: Char) {
    def move(d: Char): Beam = Beam(p = this.p.move(d), d)
  }

  val tiles = input.zipWithIndex.map { case (l, x) =>
    l.zipWithIndex.map { case (c, y) => Tile(c, Point(x, y)) }.toVector
  }.toVector

  val (maxX, maxY) = (tiles.length, tiles.head.length)

  @tailrec
  def runBeam(tiles: Mat2D, beams: List[Beam]): Mat2D = {
    if (beams.isEmpty) tiles else {
      val beam = beams.head
      val tile = tiles(beam.p.x)(beam.p.y)

      val (newTile, newBeams) = (tile.tpe, beam.dir) match {
        case ('.', d) =>
          (tile.copy(energised = tile.energised + d), List(beam.move(d)))

        case ('-', d) if Seq('L', 'R').contains(d) =>
          (tile.copy(energised = tile.energised + d), List(beam.move(d)))
        case ('-', _) =>
          (tile.copy(energised = tile.energised ++ Set('L', 'R')), List(beam.move('L'), beam.move('R')))

        case ('|', d) if Seq('U', 'D').contains(d) =>
          (tile.copy(energised = tile.energised + d), List(beam.move(d)))
        case ('|', _) =>
          (tile.copy(energised = tile.energised ++ Set('U', 'D')), List(beam.move('U'), beam.move('D')))

        case ('/', 'U') => (tile.copy(energised = tile.energised + 'R'), List(beam.move('R')))
        case ('/', 'D') => (tile.copy(energised = tile.energised + 'L'), List(beam.move('L')))
        case ('/', 'R') => (tile.copy(energised = tile.energised + 'U'), List(beam.move('U')))
        case ('/', 'L') => (tile.copy(energised = tile.energised + 'D'), List(beam.move('D')))

        case ('\\', 'U') => (tile.copy(energised = tile.energised + 'L'), List(beam.move('L')))
        case ('\\', 'D') => (tile.copy(energised = tile.energised + 'R'), List(beam.move('R')))
        case ('\\', 'R') => (tile.copy(energised = tile.energised + 'D'), List(beam.move('D')))
        case ('\\', 'L') => (tile.copy(energised = tile.energised + 'U'), List(beam.move('U')))
      }

      if (newTile != tile) {
        val updatedTiles = tiles.updated(tile.p.x, tiles(tile.p.x).updated(tile.p.y, newTile))
        val filteredBeams = newBeams.filter(b => b.p.x >= 0 && b.p.x < maxX && b.p.y >= 0 && b.p.y < maxY)
        runBeam(updatedTiles, (beams.tail ++ filteredBeams).distinct)
      } else {
        runBeam(tiles, beams.tail)
      }
    }
  }

  val energisedTiles = runBeam(tiles, List(Beam(Point(0, 0), 'R')))
  val result1 = energisedTiles.map(l => l.count(_.energised.nonEmpty)).sum

  println(s"Result part 1: $result1")

  // Part 2

  val startsX = 0.until(maxX).map(x => List(Beam(Point(x, 0), 'R'), Beam(Point(x, maxY - 1), 'L')))
  val startsY = 0.until(maxY).map(y => List(Beam(Point(0, y), 'D'), Beam(Point(maxX - 1, y), 'U')))

  val starts = (startsX.toList ++ startsY.toList).flatten

  def checkEnergised(t: Mat2D, startBeam: Beam): Int = {
    val energisedTiles = runBeam(t, List(startBeam))
    energisedTiles.map(l => l.count(_.energised.nonEmpty)).sum
  }

  val all = starts.map(b => b -> checkEnergised(tiles, b))

  val result2 = all.maxBy(_._2)._2

  println(s"Result part 2: $result2")
}

