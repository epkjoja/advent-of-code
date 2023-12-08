package se.joja.aoc.year2023

import se.joja.aoc.getInput

object Day6 extends App {

  val timeRow :: distRow :: _ = getInput("2023/day6.txt")

  val times = timeRow.split(':').last.split(' ').filter(_.nonEmpty).map(_.toLong)
  val dists = distRow.split(':').last.split(' ').filter(_.nonEmpty).map(_.toLong)

  val races = times.zip(dists).toList

  def raceFunc(raceTime: Long, holdTime: Long): Long = {
    val timeLeftToMove = raceTime - holdTime
    val speed = holdTime
    speed * timeLeftToMove
  }

  val allResults = races.map { case (time, record) =>
    0L.to(time).map(t => raceFunc(time, t)).toList -> record
  }

  val result1 = allResults.map(r => r._1.count(_ > r._2)).product

  println(s"Result part 1: ${result1}")

  // Part 2

  val raceTime = times.mkString("").toLong
  val record = dists.mkString("").toLong

  case class RaceTry(hold: Long, dist: Long)

  def findFirstCrossing(t1: RaceTry, t2: RaceTry, record: Long): (RaceTry, RaceTry) = {
    val absDiff = (t1.hold - t2.hold).abs
    if (absDiff <= 1) (t1, t2) else {
      val testHold = t1.hold + absDiff / 2
      raceFunc(raceTime, testHold) match {
        case dist if dist < record => findFirstCrossing(RaceTry(testHold, dist), t2, record)
        case dist if dist > record => findFirstCrossing(t1, RaceTry(testHold, dist), record)
      }
    }
  }

  findFirstCrossing(RaceTry(0L, raceTime / 2), RaceTry(0L, raceTime / 2), record)

  val result2 = s"$raceTime, $record"
  println(s"Result part 2: ${result2}")
}

