package se.joja.aoc.year2020

import se.joja.joja.getInput

import scala.annotation.tailrec

object Day23 extends App {

  val input = getInput("2020/day23.txt").head.toList.map(c => Integer.parseInt(c.toString))

  case class CircularQueue(elem: List[Int]) {
    val size: Int             = elem.length
    val pickupVals: List[Int] = List(1, 2, 3).map(elem(_))

    def subOne(v: Int): Int = if ((v - 1) < 1) elem.max else v - 1

    def doMove(): CircularQueue = {
      println(pickupVals.foldLeft("pickup:")((acc, e) => s"$acc $e"))
      val destVal = findDestVal(subOne(elem.head))
      println(s"destination: $destVal")

      val (prefix, postfix) = elem.drop(4).span(_ != destVal)
      val newElem           = prefix ++ List(postfix.head) ++ pickupVals ++ postfix.tail :+ elem.head
      CircularQueue(newElem)
    }

    def findDestVal(testVal: Int): Int =
      if (pickupVals.contains(testVal))
        findDestVal(subOne(testVal))
      else
        testVal

    override def toString: String =
      elem.tail.take(10).foldLeft(s"cups: (${elem.head})")((acc, e) => s"$acc $e")
  }

  @tailrec
  def doMoves(c: CircularQueue, max: Int = 1, count: Int = 1): CircularQueue = {
    if (count <= max) {
      println(s"\n-- Move $count -- ")
      println(c)
      doMoves(c.doMove(), max, count = count + 1)
    } else c
  }

//  val fin = doMoves(CircularQueue(input), 100)

//  println("\n-- final --")
//  println(fin)

  // Part two

  val data = input ++ (input.max + 1).to(1000)
  val fin2 = doMoves(CircularQueue(data), 20)

  println("\n-- final --")
  println(fin2)

}
