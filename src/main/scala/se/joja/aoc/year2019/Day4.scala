package se.joja.aoc.year2019

object Day4 extends App {

  val input = 307237.to(769058).map(_.toString).toList

  def check(in: String): Option[String] = {
    val allPairs = in.sliding(2).map(p => p.head -> p.last).toList
    val repeated = allPairs.exists(pair => pair._1 == pair._2)
    val decreasing = allPairs.exists(pair => pair._1 > pair._2)
    if (repeated && !decreasing) Some(in) else None
  }

  val valid = input.flatMap(check)
  println(s"Result: ${valid.size}")

  // Part two

  def check2(in: String): Option[String] = {
    val grouped = in.groupBy(c => c).map(x => x._1 -> x._2.size)
    if (grouped.exists(_._2 == 2)) Some(in) else None
  }

  val valid2 = valid.flatMap(check2)
  println(valid2)
  println(s"Result2: ${valid2.size}")
}
