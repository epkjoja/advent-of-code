package se.joja.aoc.year2021

import se.joja.aoc.getInput

object Day10 extends App {

  val input = getInput("2021/day10.txt")
  println(input)

  // Part 1

  // () - 0, [] - 1, {} - 2, <> - 3
  val mapping = Map('(' -> 0, '[' -> 1, '{' -> 2, '<' -> 3, ')' -> 4, ']' -> 5, '}' -> 6, '>' -> 7)

  case class State(stack: List[Int] = List.empty, pos: Int = 0, invalidToken: Option[Int] = None) {
    def add(token: Char): State = {
      val tNum = mapping(token)
      (tNum, stack) match {
        case (tn, s) if tn < 4             => State(s :+ tn, pos + 1)
        case (tn, s) if (tn - 4) == s.last => State(s.init, pos + 1)
        case (tn, s)                       => State(s, pos + 1, Some(tn))
      }
    }

    def errorScore: Int = invalidToken match {
      case Some(4) => 3
      case Some(5) => 57
      case Some(6) => 1197
      case Some(7) => 25137
      case None    => 0
    }

    def incompleteScore: Long =
      if (invalidToken.isEmpty) stack.reverse.foldLeft(0L) { case (acc, tNum) => acc * 5 + tNum + 1 } else 0
  }

  def checkLine(l: String): State = l.foldLeft(State()) {
    case (state, c) =>
      if (state.invalidToken.isEmpty) state.add(c) else state
  }

  val states = input.map(checkLine)
  println(states.mkString("\n"))

  val result1 = states.map(_.errorScore).sum
  println(s"Result part 1: $result1")

  // Part 2

  val incomplete = states.filter(_.invalidToken.isEmpty)

  val incScores = incomplete.map(_.incompleteScore)
  println(incScores)

  val middleScore = incScores.sorted.drop(incScores.size / 2).head
  println(s"Result part 2: $middleScore")

}
