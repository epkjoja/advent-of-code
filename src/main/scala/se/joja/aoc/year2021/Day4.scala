package se.joja.aoc.year2021

import se.joja.aoc.{getInput, splitOnEmptyLine}

object Day4 extends App {

  val input = getInput("2021/day4.txt")

  println(input)

  // Part 1

  val numbers = input.head.split(',').map(_.toInt).toList

  case class Board(grid: List[Int], marked: List[Boolean] = List.fill(25)(false), lastMarked: Int = 0) {
    def markNumber(n: Int): Board = grid.indexOf(n) match {
      case -1 => this
      case i  => this.copy(marked = marked.updated(i, true), lastMarked = n)
    }

    def score: Int =
      grid.zip(marked).filterNot(_._2).map(_._1).sum * lastMarked

    def hasBingo: Boolean = {
      val row = marked.grouped(5).foldLeft(0, None: Option[Int]) {
        case ((i, acc), row) =>
          if (row.forall(_ == true)) i + 1 -> Some(i) else i + 1 -> acc
      }
      val transposed = for {
        i <- 0 until (5)
        j <- 0 until (5)
      } yield marked(j * 5 + i)

      val col = transposed.grouped(5).foldLeft(0, None: Option[Int]) {
        case ((i, acc), col) =>
          if (col.forall(_ == true)) i + 1 -> Some(i) else i + 1 -> acc
      }

      (row._2, col._2) match {
        case (Some(r), _) => true
        case (_, Some(c)) => true
        case _            => false
      }
    }

    override def toString: String = {
      grid
        .zip(marked)
        .grouped(5)
        .map { r =>
          r.map { case (i, m) => s"${if (i < 10) " " else ""}$i${if (m) "*" else " "} " }.mkString
        }
        .mkString("\n") + s" ($lastMarked)"
    }
  }
  object Board {
    def apply(l: List[String]): Board = {
      Board(l.flatMap(_.split(' ').filterNot(_.isEmpty).map(_.toInt)))
    }
  }

  val boards = splitOnEmptyLine(input.drop(2)).map(l => Board(l))

  val (res, finalBoards) = numbers.foldLeft(None: Option[Board], boards) {
    case ((res, b), n) =>
      if (res.isDefined) res -> b
      else {
        val newBoards = b.map(_.markNumber(n))
        val bingo     = newBoards.find(_.hasBingo)
        bingo -> newBoards
      }
  }
  println(finalBoards.mkString("\n\n"))
  println(res.get)

  val result1 = res.get.score
  println(s"Result part 1: $result1")

  // Part 2

  val (lastToWin, finalBoards2) = numbers.foldLeft(None: Option[Board], boards) {
    case ((res, b), n) =>
      println(s"Num: $n, boards: ${b.size}")
      if (boards.isEmpty) res -> b
      else {
        val newBoards = b.map(_.markNumber(n))
        val bingo     = newBoards.find(_.hasBingo)
        bingo.orElse(res) -> newBoards.filterNot(_.hasBingo)
      }
  }
  println(finalBoards2.mkString("\n\n"))
  println(lastToWin.get)

  val result2 = lastToWin.get.score

  println(s"Result part 2: $result2")
}
