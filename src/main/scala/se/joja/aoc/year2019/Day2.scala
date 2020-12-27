package se.joja.aoc.year2019

import se.joja.joja.getInput

import scala.annotation.tailrec

object Day2 extends App {
  val input = getInput("2019/day2.txt").head.split(",").map(_.toInt).toList

  def executeOne(code: List[Int], codePtr: Int): (List[Int], Option[Int]) = {
    println(s"Code: $code, Ptr: $codePtr")
    code(codePtr) match {
      case 1 =>
        val newValue = code(code(codePtr + 1)) + code(code(codePtr + 2))
        val writePos = code(codePtr + 3)
        code.updated(writePos, newValue) -> Some(codePtr + 4)
      case 2 =>
        val newValue = code(code(codePtr + 1)) * code(code(codePtr + 2))
        val writePos = code(codePtr + 3)
        code.updated(writePos, newValue) -> Some(codePtr + 4)
      case 99 =>
        code -> None
    }
  }

  @tailrec
  def run(code: List[Int], codePtr: Int = 0): List[Int] = {
    executeOne(code, codePtr) match {
      case (newCode, Some(newPtr)) => run(newCode, newPtr)
      case (finalCode, None) => finalCode
    }
  }

  def addInParams(in: List[Int], noun: Int, verb: Int): List[Int] = {
    in.updated(1, noun).updated(2, verb)
  }

  val result = run(addInParams(input, 12, 2)).head
  println(result)

  // Part two

  val all = for {
    x <- 0.until(99)
    y <- 0.until(99)
  } yield x -> y

  val allRes = all.toList.map(in => run(addInParams(input, in._1, in._2)).head -> in).toMap

  val foundRes = allRes(19690720)

  println(100 * foundRes._1 + foundRes._2)
}
