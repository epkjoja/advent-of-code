package se.joja.aoc.year2020

import breeze.numerics.pow
import se.joja.joja.{getInput, groupInputOnStarting}

object Day14 extends App {

  val input = getInput("2020/day14.txt")

  val data = groupInputOnStarting(input)(_.startsWith("mask"))

  def calcWrite(mask: String, value: Long): Long = {
    val binStr = value.toBinaryString.reverse.zipAll(mask.reverse, '0', 'X').map {
      case (_, '0') => '0'
      case (_, '1') => '1'
      case (c, 'X') => c
    }
    val str = binStr.reverse.mkString
    //println(s"Value:  ${value.toBinaryString.padTo(36, '0')}")
    //println(s"Maks:   $mask")
    //println(s"Result: $str")
    java.lang.Long.parseUnsignedLong(str, 2)
  }

  val MemRow = """mem\[(\d+)] = (\d+)""".r

  val allWrites = data.flatMap { group =>
    val (maskRows, memRows) = group.splitAt(1)

    val mask = maskRows.head.stripPrefix("mask = ")
    memRows.map {
      case MemRow(addr, value) => addr.toLong -> calcWrite(mask, value.toLong)
    }
  }.toMap

  println(allWrites)

  val res = allWrites.values.sum
  println(s"Result: $res")

  // Part two

  def calcAdresses(mask: String, adress: Long): List[Long] = {
    val binStr = adress.toBinaryString.reverse.zipAll(mask.reverse, '0', 'X').map {
      case (c, '0') => c
      case (_, '1') => '1'
      case (_, 'X') => 'X'
    }
    val str = binStr.reverse.toList
    val num = Math.pow(2, str.count(_ == 'X')).toInt

    println(s"Str: $str, $num")
    (0 until num).toList.map { oneVar =>
      val bits = oneVar.toBinaryString.reverse.padTo(num, '0').toList

      println(s"Bits: $oneVar, $bits\n$str")
      val res = str.foldLeft(("", bits)) {
        case ((acc, bits), 'X') => (acc + bits.head, bits.tail)
        case ((acc, bits), c)   => (acc + c, bits)
      }._1

      java.lang.Long.parseUnsignedLong(res, 2)
    }
  }

  val allWrites2 = data.flatMap { group =>
    val (maskRows, memRows) = group.splitAt(1)

    val mask = maskRows.head.stripPrefix("mask = ")
    memRows.flatMap {
      case MemRow(addr, value) => calcAdresses(mask, addr.toLong).map(_  -> value.toLong)
    }
  }.toMap

  println(allWrites2)

  val res2 = allWrites2.values.sum
  println(s"Result2: $res2")

}
