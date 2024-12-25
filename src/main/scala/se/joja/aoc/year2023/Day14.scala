package se.joja.aoc.year2023

import se.joja.aoc.getInput

import scala.annotation.tailrec

object Day14 extends App {

  val input = getInput("2023/day14.txt")

  val pattern = input.map(_.toVector).toVector

  def rollLineTowardsStart(line: Vector[Char]) = {
    line.mkString("!", "", "!").split('#').map { group =>
      val (stones, spaces) = group.partition(_ == 'O')
      stones + spaces
    }.mkString("#").replace("!", "").toVector
  }

  val rolledPattern = pattern.transpose.map(rollLineTowardsStart).transpose

  def northLoad(p: Vector[Vector[Char]]): Int =
    p.reverse.zipWithIndex.map { case (line, i) => line.count(_ == 'O') * (i + 1)}.sum

  val result1 = northLoad(rolledPattern)

  println(s"Result part 1: \n${result1}")

  // Part 2

  def rotateCCW(p: Vector[Vector[Char]]) = p.transpose.reverse
  def rotateCW(p: Vector[Vector[Char]]) = p.transpose.map(_.reverse)

  def oneCycle(p: Vector[Vector[Char]]) = {
    val r1 = rotateCCW(p).map(rollLineTowardsStart) // Luta Norr
    val r2 = rotateCW(r1).map(rollLineTowardsStart) // Luta Väst
    val r3 = rotateCW(r2).map(rollLineTowardsStart) // Luta Söder
    val r4 = rotateCW(r3).map(rollLineTowardsStart) // Luta Öster
    rotateCW(rotateCW(r4))
  }

  @tailrec
  def findRepeat(p: Vector[Vector[Char]], hashes: List[String] = List.empty): (Int, Int) = {
    val hash = p.map(_.mkString("")).mkString("\n")
    println(s"Efter cycle ${hashes.length} - ${northLoad(p)}\n$hash")

    val index = hashes.indexOf(hash)
    if (index >= 0) (index, hashes.length - index) else {
      findRepeat(oneCycle(p), hashes :+ hash)
    }
  }

  val (prelude, repeat) = findRepeat(pattern)
  println(s"Prelude: $prelude, Repeat: $repeat")

  val mod = (1000000000L - prelude) % repeat
  println(s"Mod: $mod")

  val afterRepeat = 0L.until(prelude + mod).foldLeft(pattern) { case (acc, _) => oneCycle(acc) }

  val result2 = northLoad(afterRepeat)

  println(s"Result part 2: \n$result2")
}

