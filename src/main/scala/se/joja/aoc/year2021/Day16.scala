package se.joja.aoc.year2021

import se.joja.aoc.getInput
import se.joja.aoc.year2021.Day16.Operator.parse

object Day16 extends App {

  val input = getInput("2021/day16.txt").head.flatMap { c =>
    val i = Integer.parseInt(c.toString, 16).toBinaryString
    val padded = "0".repeat(4 - i.length) + i
    //println(s"$c - $padded")
    padded
  }
  println(input)

  // Part 1

  sealed trait Packet {
    def versionSum: Int
  }
  object Packet {
    def parse(s: String): (Packet, String) = {
      s.slice(3, 6) match {
        case "100" => Literal.parse(s)
        case _     => Operator.parse(s)
      }
    }
  }

  case class Literal(ver: Int, num: Long) extends Packet {
    override def versionSum: Int = ver
  }
  object Literal {
    def parse(s: String): (Packet, String) = {
      val version = Integer.parseInt(s.take(3), 2)
      val tpe     = s.slice(3, 6)
      if (tpe != "100") throw new RuntimeException(s"Unexpected type: $tpe")
      val (ints, rest) = parseNumber(s.drop(6))
      println(s"ins: $ints")
      val n = ints.zipWithIndex.foldRight(0L) { case ((s, i), acc) =>
        (Integer.parseInt(s, 2) * Math.pow(16, i) + acc).toLong
      }
      Literal(version, n) -> rest
    }

    def parseNumber(s: String): (List[String], String) = {
      println(s"parseNum: $s")
      s.head match {
        case '0' => List(s.slice(1, 5)) -> s.drop(5)
        case '1' =>
          val (list, rest) = parseNumber(s.drop(5))
          (List(s.slice(1, 5)) ++ list) -> rest
      }
    }
  }

  case class Operator(ver: Int, packets: List[Packet]) extends Packet {
    override def versionSum: Int = ver + packets.map(_.versionSum).sum
  }
  object Operator {
    def parse(s: String): (Packet, String) = {
      val version = Integer.parseInt(s.take(3), 2)
      val tpe     = s.slice(3, 6)
      if (tpe == "100") throw new RuntimeException(s"Unexpected type: $tpe")
      val lengthType = s.slice(6, 7)
      lengthType match {
        case "0" =>
          val length       = Integer.parseInt(s.slice(7, 22), 2)
          println(s"Zero - length: $length")
          val packetString = s.slice(22, 22 + length)
          Operator(version, parseZeroPacket(packetString)) -> s.drop(22 + length)
        case "1" =>
          val numSubPackets = Integer.parseInt(s.slice(7, 18), 2)
          println(s"One - num: $numSubPackets")
          val (list, rest)  = parseOnePacket(s.drop(18), numSubPackets)
          Operator(version, list) -> rest
        case l => throw new RuntimeException(s"Unexpected length type: $l")
      }
    }

    def parseZeroPacket(s: String): List[Packet] = {
      println(s"parseZeroPacket: $s")
      Packet.parse(s) match {
        case (packet, "")  => List(packet)
        case (packet, str) => List(packet) ++ parseZeroPacket(str)
      }
    }

    def parseOnePacket(s: String, num: Int): (List[Packet], String) = {
      println(s"parseOnePacket: $s, $num")
      num match {
        case 0 => List.empty -> s
        case n =>
          val (p, rest)     = Packet.parse(s)
          val (list, rest2) = parseOnePacket(rest, n - 1)
          List(p) ++ list -> rest2
      }
    }
  }

  val (packet, rest) = Packet.parse(input)
  println(packet)
  println(packet.versionSum)

  //println(s"Result part 1: $result1")

  // Part 2

  //println(s"Result part 2: $result2")

}
