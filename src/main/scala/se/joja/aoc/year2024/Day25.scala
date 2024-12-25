package se.joja.aoc.year2024

import se.joja.aoc.{readInput, splitOnEmptyLine}

object Day25 extends App {

  sealed trait KeyLock extends Product with Serializable

  case class Lock(pins: List[Int]) extends KeyLock {
    def fits(key: Key): Boolean =
      pins.zip(key.pins).forall { case (pinL, pinK) => (pinK + pinL) <= 5 }
  }

  case class Key(pins: List[Int]) extends KeyLock

  val input = splitOnEmptyLine(readInput(2024, 25)).map { pattern =>
    val trans = pattern.transpose
    if (pattern.head == "#####")
      Lock(trans.map(_.count(_ == '#') - 1))
    else
      Key(trans.map(_.count(_ == '#') - 1))
  }

  val locks = input.collect { case l @ Lock(_) => l }
  val keys = input.collect { case k @ Key(_) => k }

  val j = for {
    key <- keys
    lock <- locks
  } yield lock.fits(key)

  val result1 = j.count(identity)

  println(s"Result part 1: $result1")

  // Part 2

  val result2 = ""

  println(s"Result part 2: $result2")
}

