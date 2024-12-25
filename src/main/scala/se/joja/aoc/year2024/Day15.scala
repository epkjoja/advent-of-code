package se.joja.aoc.year2024

import se.joja.aoc.util.{Area, Point}
import se.joja.aoc.{readInput, splitOnEmptyLine}

object Day15 extends App {

  val mapRaw :: movesRaw :: Nil = splitOnEmptyLine(readInput(2024, 15, true))
  val mapPoints = mapRaw.zipWithIndex.flatMap { case (line, x) =>
    line.zipWithIndex.map { case (c, y) =>
      Point(x, y) -> c
    }
  }
  val area = Area(mapPoints, '.')
  val moves = movesRaw.flatMap(_.toList)

  def move(a: Area[Char], p: Point, dir: Char): Option[Area[Char]] = {
    val newP = p.move(dir)
    val newA = a(newP) match {
      case '#' => None
      case 'O' => move(a, newP, dir)
      case '.' => Some(a)
    }
    newA.map(x => x.updated(newP, x(p)).updated(p, x(newP)))
  }

  val fin = moves.foldLeft(area) { case (acc, dir) =>
//    println(s"Move: $dir\n$acc\n")
    val robot = acc.filter(_ == '@').head._1
    move(acc, robot, dir).getOrElse(acc)
  }

  val res = fin.filter(_ == 'O').map { case (p, _) => p.x * 100L + p.y}

  val result1 = res.sum

  println(s"Result part 1: $result1")

  // Part 2

  val area2 = area.copy(a = area.a.map { l =>
    l.mkString.replace("#", "##").replace("O", "[]").replace(".", "..").replace("@", "@.").toVector
  })

  def move2(a: Area[Char], p: Point, dir: Char): Option[Area[Char]] = {
    val newP = p.move(dir)
    val newA = (a(newP), dir) match {
      case ('#', _) => None
      case ('.', _) => Some(a)
      case ('[', '>') => move(a, newP.right, dir)
      case ('[', '^') => move(a, newP.right, dir)
      case ('[', '>') => move(a, newP.right, dir)
    }
    newA.map(x => x.updated(newP, x(p)).updated(p, x(newP)))
  }

  val fin2 = moves.foldLeft(area) { case (acc, dir) =>
    //    println(s"Move: $dir\n$acc\n")
    val robot = acc.filter(_ == '@').head._1
    move(acc, robot, dir).getOrElse(acc)
  }

  val result2 = area2

  println(s"Result part 2: $result2")
}

