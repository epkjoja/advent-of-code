package se.joja.aoc.year2020

import se.joja.joja.{chineseRemainder, getInput}

object Day13 extends App {

  val input = getInput("2020/day13.txt")

  val earliest = input.head.toInt
  val busIds   = input(1).split(',').toList.filterNot(_ == "x").map(_.toInt)

  val depart = busIds.map { busId =>
    busId -> (earliest / busId * busId + busId)
  }.toMap

  val bus = depart.minBy(_._2)

  println(s"Found bus: $bus")

  val res = bus._1 * (bus._2 - earliest)
  println(s"Result: $res")

  // Part two

  val buses = input(1).split(',').zipWithIndex.filterNot(_._1 == "x").map(i => i._1.toLong -> i._2).toList

  val maxStep = buses.maxBy(_._1)

  println(s"Max step: $maxStep")

  def testOneTimestamp(ts: Long): Boolean = {
    //println(s"Testing ts: $ts")
    buses.forall {
      case (busId, dt) =>
        val even = if (ts % busId == 0) 0 else busId
        val a    = ts / busId * busId + even
        val b    = ts + dt
        //println(s"* Testing bus: $busId, $dt -> $a, $b")
        a == b
    }
  }

  //println(testOneTimestamp(1068781))
  val start = 100000000000000L / maxStep._1 * maxStep._1 - maxStep._2
  val start2 = maxStep._1 - maxStep._2

  //val res2 =
  //  LazyList.iterate(start)(old => old + maxStep._1).dropWhile(ts => !testOneTimestamp(ts)).take(1).toList

  private val crt = chineseRemainder(buses.map(_._1), buses.map(b => b._1 - b._2))

  println(s"Result2: $crt")
}
