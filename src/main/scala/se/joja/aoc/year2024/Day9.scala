package se.joja.aoc.year2024

import se.joja.aoc.readInput

import scala.annotation.tailrec

object Day9 extends App {

  val input = readInput(2024, 9).head.map(_.asDigit)

  case class Pos(start: Int, size: Int, num: Int = -1) {
    def fillEmpty(n: Int): Pos = Pos(start + n, size - n, num)
    def takePart(n: Int): Pos = Pos(start, size - n, num)
  }

  object Pos {
    implicit val ord: Ordering[Pos] = Ordering.by(unapply)
  }


  var pos = 0
  val allPos = input.zipWithIndex.flatMap { case (size, i) =>
    if (size == 0) None else {
      val p = Pos(pos, size, if (i % 2 == 0) i / 2 else -1)
      pos += size
      Some(p)
    }
  }.toList

  val (filled, empty) = allPos.partition(_.num >= 0)
//  println(s"Filled0: $filled")
//  println(s"Empty0: $empty")

  @tailrec
  def compactRec(f: List[Pos], e: List[Pos]): (List[Pos], List[Pos]) = {
//    println(printMem(f ++ e))
//    println(s"Filled: $f")
//    println(s"Empty: $e")

    if ((f.last.start + f.last.size) <= e.head.start) f -> e else {
      val (restF, lastF) = (f.init, f.last)
      val (firstE, restE) = (e.head, e.tail)

      val (newF, newE) = (lastF.size, firstE.size) match {
        case (sizeF, sizeE) if sizeF == sizeE =>
          (restF :+ lastF.copy(start = firstE.start)) -> (restE :+ lastF.copy(num = -1))

        case (sizeF, sizeE) if sizeE > sizeF =>
          (restF :+ lastF.copy(start = firstE.start)) -> (firstE.fillEmpty(sizeF) +: restE :+ lastF.copy(num = -1))

        case (sizeF, sizeE) if sizeF > sizeE =>
          val diff = sizeF - sizeE
          (restF ++ List(firstE.copy(num = lastF.num), lastF.takePart(sizeE))) -> (restE :+ Pos(lastF.start + diff, sizeE))
      }

      compactRec(newF.sorted, newE.sorted)
    }
  }

  def printMem(list: List[Pos]): String = {
    list.sorted.map {
      case Pos(_, size, -1) => ".".repeat(size)
      case Pos(_, size, num) => s"$num".repeat(size)
    }.mkString("")
  }

  def checksum(list: List[Pos]): Long = {
    list.map {
      case Pos(_, _, -1) => 0L
      case Pos(start, size, num) =>
        (start.until(start + size)).sum * num.toLong
    }.sum
  }

  val (finFilled, _) = compactRec(filled, empty)

  val result1 = checksum(finFilled)

  println(s"Result part 1: $result1")

  // Part 2

  def tryMove(num: Int, f: List[Pos], e: List[Pos]): (List[Pos], List[Pos]) = {
    val (filtered, restF) = f.partition(_.num == num)

    if (filtered.isEmpty) (f, e) else {
      val foundF = filtered.head

      e.find(ep => ep.size >= foundF.size && ep.start < foundF.start) match {
        case None => (f, e)
        case Some(foundE) =>
          val restE = e.filterNot(_.start == foundE.start)
          val diff = foundE.size - foundF.size
          val partE = if (diff > 0) Some(foundE.fillEmpty(foundF.size)) else None
          val f2 = restF :+ foundF.copy(start = foundE.start)
          val e2 = restE ++ List(Some(foundF.copy(num = -1)), partE).flatten
          (f2.sorted, e2.sorted)
      }
    }
  }

  val maxNum = filled.maxBy(_.num).num

  def moveRec(num: Int, f: List[Pos], e: List[Pos]): (List[Pos], List[Pos]) = {
//    println(s"Num: $num, ${printMem(f ++ e)}")
    if (num < 0) (f, e) else {
      val (newF, newE) = tryMove(num, f, e)
      moveRec(num - 1, newF, newE)
    }
  }

  val (f2, e2) = moveRec(maxNum, filled, empty)

  val result2 = checksum(f2 ++ e2)

  println(s"Result part 2: $result2")
}

