package se.joja.aoc.year2022

import fs2._
import se.joja.aoc.getInput

import scala.annotation.tailrec
import scala.collection.mutable

object Day17 extends App {

  val input    = getInput("2022/day17.txt").head
  val inLength = input.length

  case class P(x: Int = 0, y: Int = 0)

  sealed trait Rock {
    val pos: P = P()
    val width: Int

    def tryFall(tops: Seq[Int]): Option[Rock] = {
      //println(s"Try fall - pos: $pos")
      val newRock = this match {
        case Dash(pos) => Dash(P(pos.x, pos.y - 1))
        case Plus(pos) => Plus(P(pos.x, pos.y - 1))
        case InvL(pos) => InvL(P(pos.x, pos.y - 1))
        case I(pos)    => I(P(pos.x, pos.y - 1))
        case Dot(pos)  => Dot(P(pos.x, pos.y - 1))
      }

      if (newRock.isHitting(tops)) None else Some(newRock)
    }

    def blow(iter: Iterator[Char], tops: Seq[Int]): Rock = {
      val dir = iter.next() match {
        case '>' => 1
        case '<' => -1
        case o   => throw new RuntimeException(s"Unknown direction '$o'")
      }

      val tryBlow = this match {
        case Dash(pos) => Dash(P(pos.x + dir, pos.y))
        case Plus(pos) => Plus(P(pos.x + dir, pos.y))
        case InvL(pos) => InvL(P(pos.x + dir, pos.y))
        case I(pos)    => I(P(pos.x + dir, pos.y))
        case Dot(pos)  => Dot(P(pos.x + dir, pos.y))
      }

      if (tryBlow.isInside(tops.size) && !tryBlow.isHitting(tops)) tryBlow else this
    }

    def isInside(size: Int): Boolean =
      pos.x >= 0 && pos.x + width <= size

    def isHitting(tops: Seq[Int]): Boolean

    def init(topY: Int): Rock = {
      val b = this match {
        case Dash(_) => Dash(P(2, topY + 5))
        case Plus(_) => Plus(P(2, topY + 7))
        case InvL(_) => InvL(P(2, topY + 7))
        case I(_)    => I(P(2, topY + 8))
        case Dot(_)  => Dot(P(2, topY + 6))
      }
      println(s"Init: $b")
      b
    }

    def topBlocks: List[P] =
      this match {
        case Dash(pos) => 0.to(3).map(i => pos.copy(x = pos.x + i)).toList
        case Plus(p)   => List(P(p.x, p.y - 1), P(p.x + 1, p.y), P(p.x + 2, p.y - 1))
        case InvL(p)   => List(P(p.x, p.y - 2), P(p.x + 1, p.y - 2), P(p.x + 2, p.y))
        case I(p)      => List(p)
        case Dot(p)    => List(p, P(p.x + 1, p.y))
      }
  }

  case class Dash(override val pos: P = P()) extends Rock {
    override val width: Int = 4
    def isHitting(tops: Seq[Int]): Boolean =
      0.to(3).exists(i => tops(pos.x + i) >= pos.y)
  }

  case class Plus(override val pos: P = P()) extends Rock {
    override val width: Int = 3
    def isHitting(tops: Seq[Int]): Boolean =
      tops(pos.x) >= pos.y - 1 || tops(pos.x + 1) >= pos.y - 2 || tops(pos.x + 2) >= pos.y - 1
  }

  case class InvL(override val pos: P = P()) extends Rock {
    override val width: Int = 3
    def isHitting(tops: Seq[Int]): Boolean =
      0.to(2).exists(i => tops(pos.x + i) >= pos.y - 2)
  }

  case class I(override val pos: P = P()) extends Rock {
    override val width: Int = 1
    def isHitting(tops: Seq[Int]): Boolean =
      tops(pos.x) >= pos.y - 3
  }

  case class Dot(override val pos: P = P()) extends Rock {
    override val width: Int = 2
    def isHitting(tops: Seq[Int]): Boolean =
      0.to(1).exists(i => tops(pos.x + i) >= pos.y - 1)
  }

  val rockList: List[Rock] = List(Dash(), Plus(), InvL(), I(), Dot())
  val rocks = Stream.iterate(0)(_ + 1).map { i =>
    println(s"Rock stream: $i")
    rockList(i % 5)
  }
  val jets = Iterator.from(0).map { i =>
    val c = input(i % inLength)
    println(s"Dir: $c")
    c
  }

  val tops = mutable.Seq.fill(7)(0)

  @tailrec
  def dropRock(r: Rock, tops: Seq[Int]): Rock = {
    println(s"Drop rock: $r")
    r.tryFall(tops) match {
      case Some(r2) => dropRock(r2.blow(jets, tops), tops)
      case None     => r
    }
  }

  rocks
    .take(2022)
    .map { r =>
      println(s"Tops: $tops")
      val stopped = dropRock(r.init(tops.max), tops.toSeq)
      println(s"Stopped: $stopped")
      stopped.topBlocks.foreach { p =>
        if (tops(p.x) < p.y) tops.update(p.x, p.y)
      }
    }
    .compile
    .drain

  println(tops)

  //println(s"Result part 1: $result1")

  // Part 2

  //println(s"Result part 2: $result2")
}
