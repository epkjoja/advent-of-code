package se.joja.aoc.year2020

import se.joja.joja._

object Day7 extends App {

  val input = getInput("2020/day7.txt")

  val outerRe = """(\w+ \w+) bags contain (.*).""".r
  val innerRe = """(\d+) (\w+ \w+) bags?""".r

  val allBags = input.map {
    case outerRe(outer, rest) =>
      val innerBags = rest.split(",").toList.map(_.trim).flatMap {
        case innerRe(num, inner) => Some(inner -> num.toLong)
        case "no other bags"     => None
      }.toMap

      outer -> innerBags
  }.toMap

  val allBagsOnlyColor = allBags.map(bag => bag._1 -> bag._2.keys.toList)


  val data2 = invertMap(allBagsOnlyColor)

  def canContain(color: String): Set[String] = {
    val canBeIn = data2.getOrElse(color, List.empty)
    (canBeIn ++ canBeIn.flatMap(canContain)).toSet
  }

  val res = canContain("shiny gold")
  println(s"Size: ${res.size} - $res")

  // Part two

  def containedNumber(color: String): Long = {
    val inBag = allBags.getOrElse(color, Map.empty)
    inBag.foldLeft(0L) { case (acc, (innerColor, num)) =>
      containedNumber(innerColor) * num + acc
    } + inBag.values.sum
  }

  println(s"Result2: ${containedNumber("shiny gold")}")
}
