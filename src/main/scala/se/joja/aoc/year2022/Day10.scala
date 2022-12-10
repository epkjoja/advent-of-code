package se.joja.aoc.year2022

import se.joja.aoc.{getInput, splitOnEmptyLine}

object Day10 extends App {

  val input = getInput("2022/day10.txt")

  val CmdNoop = """noop""".r // Takes 1 cycle
  val CmdAddx = """addx (-?\d+)""".r // Takes 2 cycles

  case class Cycle(num: Int, xDuring: Int, xAfter: Int) {
    def execCmd(cmd: String): List[Cycle] = {
      cmd match {
        case CmdNoop() =>
          Cycle(num + 1, xAfter, xAfter) :: Nil

        case CmdAddx(value) =>
          Cycle(num + 1, xAfter, xAfter) ::
          Cycle(num + 2, xAfter, xAfter + value.toInt) :: Nil
      }
    }
  }

  val cycles = input.foldLeft(List(Cycle(0, 1, 1))) {
    case (acc, cmd) =>
      acc ++ acc.last.execCmd(cmd)
  }

  val resCycles = cycles.filter { c =>
    c.num == 20 || (c.num + 20)  % 40 == 0
  }

  println(resCycles)

  val result1 = resCycles.map(c => c.num * c.xDuring).sum

  println(s"Result part 1: $result1")

  // Part 2

  val pixels = cycles.drop(1).map { c =>
    if (((c.num - 1) % 40 - c.xDuring).abs <= 1) "#" else "."
  }

  val result2 = pixels.grouped(40).map(_.mkString).mkString("\n")

  println(s"Result part 2:\n$result2")
}
