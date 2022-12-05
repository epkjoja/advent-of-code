package se.joja.aoc.year2022

import se.joja.aoc.{getInput, splitOnEmptyLine}

object Day5 extends App {

  val colCount = 9

  val input = getInput("2022/day5.txt")

  val stackLines :: moveLines :: Nil = splitOnEmptyLine(input)

  case class Stack[A](stack: Map[Int, List[A]]) {
    def movePart1(count: Int, fromCol: Int, toCol: Int): Stack[A] = {
      val newToCol   = stack(toCol) ::: stack(fromCol).reverse.take(count)
      val newFromCol = stack(fromCol).dropRight(count)
      Stack(stack.updated(fromCol, newFromCol).updated(toCol, newToCol))
    }

    def movePart2(count: Int, fromCol: Int, toCol: Int): Stack[A] = {
      val newToCol   = stack(toCol) ::: stack(fromCol).takeRight(count)
      val newFromCol = stack(fromCol).dropRight(count)
      Stack(stack.updated(fromCol, newFromCol).updated(toCol, newToCol))
    }

    def topChars: String =
      1.to(colCount).map(i => stack(i).last).mkString
  }

  val stack = Stack(
    stackLines.reverse
      .map { line =>
        1.to(colCount).map(i => (i - 1) * 4 + 1).map { pos =>
          if (pos >= line.length) ' ' else line.charAt(pos)
        }
      }
      .transpose
      .map { col =>
        col.head.toString.toInt -> col.tail.filterNot(_ == ' ')
      }
      .toMap)

  val MovePattern = """move (\d+) from (\d+) to (\d+)""".r

  val moves = moveLines.map {
    case MovePattern(c, f, t) => (c.toInt, f.toInt, t.toInt)
  }

  val finalStack1 = moves.foldLeft(stack) { (s, m) =>
    s.movePart1(m._1, m._2, m._3)
  }

  println(s"Result part 1: ${finalStack1.topChars}")

  // Part 2

  val finalStack2 = moves.foldLeft(stack) { (s, m) =>
    s.movePart2(m._1, m._2, m._3)
  }

  println(s"Result part 2: ${finalStack2.topChars}")
}
