package se.joja.aoc.year2020

import se.joja.joja.getInput

object Day9 extends App {

  val input = getInput("2020/day9.txt").map(_.toLong)

  def checkNextIsValid(in: List[Long], next: Long): Option[(Int, Int)] = {
    val size = in.size

    val allPairs = 0.until(size).combinations(2).map(seq => (seq(0), seq(1))).toList

    //println(s"Next: $next, AllPairs: $allPairs")

    allPairs.find { case (a, b) => in(a) + in(b) == next }
  }

  val preamble = 25

  def checkInput(ptr: Int = 0, acc: List[(Int, Int)] = List.empty): List[(Int, Int)] = {
    val a = input.slice(ptr, ptr + preamble)
    val b = input(ptr + preamble)
    checkNextIsValid(a, b) match {
      case Some(value) => checkInput(ptr + 1, acc :+ value)
      case None =>
        println(s"'$b' on pos ${ptr + preamble} is not valid")
        acc
    }
  }

  checkInput()

  // Part two

  val myNumber = 1639024365L

  def checkSumFrom(data: List[Long]): Option[Int] = {
    data.zipWithIndex.foldLeft[(Option[Long], Option[Int])](Some(0L), None) {
      case ((maybeAccSum, maybePos), (currVal, currPos)) =>
        (maybeAccSum, maybePos) match {
          case (None, None) => (None, None)
          case (Some(accSum), Some(pos)) => (Some(accSum), Some(pos))
          case (Some(accSum), None) =>
            val newSum = accSum + currVal
            if (newSum == myNumber) {
              (Some(newSum), Some(currPos))
            } else if (newSum < myNumber) {
              (Some(newSum), None)
            } else {
              (None, None)
            }
        }
    }._2
  }

  input.indices.foreach { pos =>
    val res = checkSumFrom(input.drop(pos))
    res match {
      case Some(value) =>
        val finalRange: List[Long] = input.slice(pos, pos + value + 1)
        val finalRes = if (finalRange.isEmpty) 0 else finalRange.min + finalRange.max
        println(s"Found match: $pos - $value, Sum: ${finalRange.sum}")
        println(s"Range: $finalRange")
        println(s"Min: ${finalRange.min}, Max: ${finalRange.max}, Sum: $finalRes")
      case None =>
        println(s"Miss on pos: $pos")
    }
  }
}
