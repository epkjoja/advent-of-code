package se.joja.aoc.year2020

import scala.annotation.tailrec
import scala.io.Source

object Day5 extends App {

  val input = Source.fromResource("2020/day5.txt").getLines().toSeq

  def divide(in: String): Int = {

    @tailrec
    def divideInt(in: String, min: Int, max: Int): Int = {
      //println(s"divide - in: $in, min: $min, max: $max")
      val part = (max - min + 1) / 2
      in.headOption match {
        case Some('F') | Some('L') => divideInt(in.tail, min, max - part)
        case Some('B') | Some('R') => divideInt(in.tail, min + part, max)
        case None => min
      }
    }

    divideInt(in, 0, Math.pow(2, in.length).toInt - 1)
  }

  def seatId(bp: String): Int = {
    val row = divide(bp.takeWhile("FB".contains(_)))
    val seat = divide(bp.dropWhile("FB".contains(_)))
    val id = row * 8 + seat
    println(s"BP: $bp - row: $row, col: $seat, seatId: $id")
    id
  }

  val max = input.map(seatId).max
  println(s"Result: $max")

  val taken = input.map(seatId).sorted

  val free = 0.until(max).toList.diff(taken)
  println(s"Free: $free")
}
