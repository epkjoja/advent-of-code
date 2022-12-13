package se.joja.aoc.year2022

import se.joja.aoc.{getInput, splitOnEmptyLine}

object Day13 extends App {

  val input = getInput("2022/day13.txt")
  val pairs = splitOnEmptyLine(input).collect {
    case left :: right :: Nil => Packet(left) -> Packet(right)
  }

  sealed abstract class Packet[T: Ordering] extends Ordered[Packet[T]]

  case class ListP[T: Ordering](list: List[Packet[T]]) extends Packet[T] {
    override def compare(that: Packet[T]): Int =
      that match {
        case ListP(l) =>
          val c = list.zip(l).foldLeft(0) {
            case (acc, (p1, p2)) =>
              if (acc != 0) acc else p1.compare(p2)
          }
          if (c != 0) c else (list.length - l.length).sign

        case v @ ValP(_) => this.compare(ListP(List(v)))
      }

    override def toString: String = s"[${list.mkString(",")}]"
  }

  case class ValP[T: Ordering](value: T) extends Packet[T] {
    override def compare(that: Packet[T]): Int =
      that match {
        case ValP(v) => implicitly[Ordering[T]].compare(value, v)
        case l @ ListP(_) => ListP(List(this)).compare(l)
      }

    override def toString: String = value.toString
  }

  object Packet {
    def apply(s: String): Packet[Int] = parse(s)._1

    def parse(in: String): (Packet[Int], String) = {
      //println(s"Parse (packet): $in")
      in.headOption match {
        case Some(d) if d.isDigit => ValP.parse(in)
        case Some('[')            => ListP.parse(in)
        case _                    => throw new RuntimeException("")
      }
    }
  }

  object ListP {
    def parse(in: String): (Packet[Int], String) = {
      //println(s"Parse (list): $in")
      assert(in.head == '[')
      var string = in.drop(1)
      var l      = List.empty[Packet[Int]]
      while (string.head != ']') {
        val (p, rest) = Packet.parse(string)
        l = l :+ p
        string = if (rest.head == ',') rest.drop(1) else rest
      }
      (ListP(l), string.drop(1))
    }
  }

  object ValP {
    def parse(in: String): (Packet[Int], String) = {
      //println(s"Parse (val): $in")
      val (d, rest) = in.span(_.isDigit)
      (ValP(d.toInt), rest)
    }
  }

  println(pairs.map(p => s"${p._1}\n${p._2}\n").mkString("\n"))

  val result = pairs.zipWithIndex.map { case ((p1, p2), i) =>
    if (p1.compare(p2) < 0) i + 1 else 0
  }

  println(result)

  val result1 = result.sum
  println(s"Result part 1: $result1")

  // Part 2

  val markers = List("[[2]]", "[[6]]")
  val packets = (input ++ markers).collect {
    case line if line.nonEmpty => Packet(line)
  }.sorted

  println(packets.mkString("\n"))

  val markerPackets = markers.map(Packet(_))
  val result2 = packets.zipWithIndex.filter(p => markerPackets.contains(p._1)).map(_._2 + 1).product

  println(s"Result part 2: $result2")
}
