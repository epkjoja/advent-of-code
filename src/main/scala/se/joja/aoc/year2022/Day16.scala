package se.joja.aoc.year2022

import se.joja.aoc.getInput

object Day16 extends App {

  val input = getInput("2022/day16.txt")

  val InLine = """Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? ([\w, ]+)""".r

  case class Valve(id: String, rate: Int, tunnels: List[String])

  val valves = input.map {
    case InLine(id, rate, t) => Valve(id, rate.toInt, t.split(", ").toList)
  }
  println(valves.mkString("\n"))

  val valveMap  = valves.groupMapReduce(_.id)(identity)((a, _) => a)
  val maxOpened = valves.count(_.rate > 0)

  def searchBest(curr: Valve,
                 sum: Int = 0,
                 opened: List[String] = List.empty,
                 path: List[String] = List.empty,
                 timeLeft: Int = 30): (Int, List[String]) = {
    println(s"Evaluating valve: ${curr.id}, sum : $sum, time left: $timeLeft")
    if (timeLeft == 0) (sum, path)
    else if (opened.size >= maxOpened) {
      (opened.map(id => valveMap(id).rate).sum * timeLeft, path)
    } else {
      val newSum = opened.map(id => valveMap(id).rate).sum
      val toNext = curr.tunnels
        .map(t => valveMap(t))
        .filter(next => path.count(_ == next.id) <= 2)
        .map { next =>
          searchBest(next, newSum, opened, path :+ curr.id, timeLeft - 1)
        }

      val stay =
        if (curr.rate <= 0 || opened.contains(curr.id)) Nil
        else {
          List(
            searchBest(curr, newSum, opened :+ curr.id, path :+ curr.id, timeLeft - 1)
          )
        }

      (stay ++ toNext).maxByOption(_._1) match {
        case Some(value) => value
        case None        => (opened.map(id => valveMap(id).rate).sum * timeLeft, path)
      }
    }
  }

  val best = searchBest(valveMap("AA"))

  println(best)
  //println(s"Result part 1: $result1")

  // Part 2

  //println(s"Result part 2: $result2")
}
