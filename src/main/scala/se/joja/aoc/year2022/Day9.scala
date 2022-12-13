package se.joja.aoc.year2022

import se.joja.aoc.{getInput, splitOnEmptyLine}

object Day9 extends App {

  val input = getInput("2022/day9.txt")

  val moves = input.map { s =>
    val dir :: steps :: Nil = s.split(' ').toList
    Move(dir, steps.toInt)
  }

  case class Move(dir: String, steps: Int)
  case class Point(x: Int, y: Int) {
    def isTouching(other: Point): Boolean =
      (x - other.x).abs <= 1 && (y - other.y).abs <= 1

    def moveHead(dir: String): Point = dir match {
      case "L" => copy(x = x - 1)
      case "R" => copy(x = x + 1)
      case "U" => copy(y = y + 1)
      case "D" => copy(y = y - 1)
    }

    def moveTail(head: Point): Point = {
      if (isTouching(head)) this
      else {
        (head, this) match {
          case (h, t) if h.y - t.y > 1  => copy(h.x, t.y + 1) // Up
          case (h, t) if h.y - t.y < -1 => copy(h.x, t.y - 1) // Down
          case (h, t) if h.x - t.x > 1  => copy(t.x + 1, h.y) // Right
          case (h, t) if h.x - t.x < -1 => copy(t.x - 1, h.y) // Left
        }
      }
    }
  }

  case class Board(rope: List[Point]) {
    private def moveRopeOnce(dir: String): Board = {
      val newRope = rope.drop(1).foldLeft(List(rope.head.moveHead(dir))) { (acc, p) =>
        acc :+ p.moveTail(acc.last)
      }
      Board(newRope)
    }

    def moveRope(move: Move): (Board, List[Point]) = {
      val boards = 0.until(move.steps).foldLeft(List(this)) { case (boards, _) =>
          boards :+ boards.last.moveRopeOnce(move.dir)
      }

      (boards.last, boards.map(_.rope.last).distinct)
    }
  }

  val (_, finalTps1) = moves.foldLeft((Board(0.until(2).map(_ => Point(0, 0)).toList), List.empty[Point])) {
    case ((board, tailPoints), move) =>
      val (newBoard, newTps) = board.moveRope(move)
      (newBoard, tailPoints ++ newTps)
  }

  val result1 = finalTps1.distinct.length

  println(s"Result part 1: $result1")

  // Part 2

  val (_, finalTps2) = moves.foldLeft((Board(0.until(10).map(_ => Point(0, 0)).toList), List.empty[Point])) {
    case ((board, tailPoints), move) =>
      val (newBoard, newTps) = board.moveRope(move)
      (newBoard, tailPoints ++ newTps)
  }

  val result2 = finalTps2.distinct.length

  println(s"Result part 2: $result2")
}
