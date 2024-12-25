package se.joja.aoc.year2023

import se.joja.aoc.getInput

object Day15 extends App {

  val input = getInput("2023/day15.txt").head.split(',').toList

  def calcHash(s: String) = s.foldLeft(0L) { case (acc, c) =>
    ((acc + c.toLong) * 17) % 256
  }

  val result1 = input.map(calcHash).sum

  println(s"Result part 1: \n${result1}")

  // Part 2

  val DashRegex = """(\w+)-""".r
  val EqualRegex = """(\w+)=(\d)""".r

  case class Cmd(label: String, lens: Int)

  val commands = input.foldLeft(Map.empty[Long, List[Cmd]]) { case (acc, cmd) =>
    println(acc)
    cmd match {
      case DashRegex(op) =>
        val hash = calcHash(op)
        println(s"Dash, $op, [$hash]")
        acc + (hash -> acc.getOrElse(hash, List.empty).filter(_.label != op))
      case EqualRegex(op, num) =>
        val hash = calcHash(op)
        println(s"Equals, $op, [$hash] = $num")
        val list = acc.getOrElse(hash, List.empty)
        if (list.exists(_.label == op)) {
          acc + (hash -> list.map(cmd => if (cmd.label == op) cmd.copy(lens = num.toInt) else cmd))
        } else {
          acc + (hash -> (list :+ Cmd(op, num.toInt)))
        }
    }
  }

  val power = commands.map { case (box, lenses) =>
    lenses.zipWithIndex.map { case (cmd, i) => (box + 1) * (i + 1) * cmd.lens }.sum
  }.sum

  val result2 = power

  println(s"Result part 2: $result2")
}

