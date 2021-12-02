package se.joja.aoc.year2020

import se.joja.aoc.{getInput, splitOnEmptyLine}

import scala.annotation.tailrec

object Day16 extends App {

  val input = getInput("2020/day16.txt")

  val (rulesList :: myTicket :: ticketLists :: Nil) = splitOnEmptyLine(input)

  val Regex = """([\w ]+): (\d+)-(\d+) or (\d+)-(\d+)""".r

  case class Rule(name: String, r1l: Int, r1h: Int, r2l: Int, r2h: Int) {
    def isValid(value: Int): Boolean =
      value >= r1l && value <= r1h || value >= r2l && value <= r2h
  }

  val rules = rulesList.map {
    case Regex(name, r1l, r1h, r2l, r2h) => Rule(name, r1l.toInt, r1h.toInt, r2l.toInt, r2h.toInt)
  }

  val tickets = ticketLists.tail.map { t =>
    t.split(',').toList.map(_.toInt)
  }

  val invalid = tickets.map { t =>
    t.filterNot(v => rules.exists(_.isValid(v)))
  }

  println(s"Tickets: ${tickets.size}, Invalid: ${invalid.size}")
  //println(invalid.map(_.mkString(",")).mkString("\n"))

  val res = invalid.map(_.sum).sum

  println(s"Result: $res")

  // Part two

  val validTickets = tickets.filter { t =>
    t.forall(v => rules.exists(_.isValid(v)))
  }

  println(s"Valid tickets: $validTickets")

  val valsPerPos = validTickets.head.indices.map { i =>
    i -> validTickets.map(_(i))
  }.toMap

  //println(valsPerPos)

  val res2 = valsPerPos.map {case (pos, vals) =>
    pos -> rules.filter(r => vals.forall(r.isValid)).map(_.name)
  }

  //val fixed = Map("price" -> 4, "train" -> 9, "type" -> 7, "arrival station" -> 15, "zone" -> 18,
  //"arrival track" -> 5, "arrival platform" -> 2, "route" -> 14, "departure time" -> 6, "departure platform" -> 10,
  //"departure station" -> 16, "departure track" -> 13, "departure location" -> 19, "departure date" -> 11,
  //"arrival location" -> 8, "seat" -> 1, "class" -> 0, "wagon" -> 17)

  @tailrec
  def fixOne(map: Map[Int, List[String]], fixed: Map[String, Int]): Map[String, Int] = {
    if (map.isEmpty)
      fixed
    else {
      val (pos, str :: Nil) = map.find(_._2.size == 1).get
      val newMap = map.removed(pos).map { case (pos, list) => pos -> list.filterNot(_ == str)}
      fixOne(newMap, fixed + (str -> pos))
    }
  }

  val fixed = fixOne(res2, Map.empty)

  //val rest = res2.map { case (pos, vals) => pos -> vals.filterNot(fixed.contains)}.filter(_._2.size <= 2)

  println(s"Fixed: $fixed")

  val fields = fixed.filter(_._1.startsWith("departure")).values.toList
  println(fields)

  val res3 = myTicket.last.split(',').map(_.toLong).zipWithIndex.filter(x => fields.contains(x._2)).toMap
  println(res3)

  println(s"Result2: ${res3.map(_._1).product}")
}
