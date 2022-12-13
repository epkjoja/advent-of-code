package se.joja.aoc.year2022

import se.joja.aoc.{getInput, splitOnEmptyLine}

object Day13 extends App {

  val input = getInput("2022/day13.txt")
  val pairs = splitOnEmptyLine(input).collect {
    case left :: right :: Nil => Packet(left) -> Packet(right)
  }

  sealed trait Packet[T]
  case class ListP[T](list: List[Packet[T]]) extends Packet[T] {
    override def toString: String = s"[${list.mkString(",")}]"
  }

  case class ValP[T](value: T) extends Packet[T] {
    override def toString: String = value.toString
  }

  object Packet {
    def apply(s: String): Packet[Int] = parse(s)._1.head

    def parse(s: String): (List[Packet[Int]], String) = {
      println(s"Parse: $s")
      s.headOption match {
        case Some(d) if d.isDigit =>
          val v      = ValP(s.takeWhile(_.isDigit).toInt)
          val (l, r) = parse(s.dropWhile(c => c.isDigit))
          (List(v) ++ l, r)

        case Some('[') =>
          val (l, r) = parse(s.drop(1))
          (List(ListP(l)), r)

        case Some(']') =>
          (List.empty[Packet[Int]], s.drop(1))

        case Some(',') =>
          parse(s.drop(1))

        case None =>
          assert(false)
          (List.empty[Packet[Int]], "")
      }
    }
  }

  println(pairs.mkString("\n"))
  //println(s"Result part 1: $result1")

  // Part 2

  //println(s"Result part 2: $result2")
}
