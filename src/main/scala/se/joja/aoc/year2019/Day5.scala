package se.joja.aoc.year2019

import se.joja.aoc.getInput

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Day5 extends App {
  val input = getInput("2019/day5.txt").head.split(",").map(_.toInt).toList

  def executeOne(code: List[Int],
                 codePtr: Int,
                 extra: Map[String, Any] = Map.empty): (List[Int], Option[Int], Map[String, Any]) = {
    println(s"Code: ${code.slice(codePtr, codePtr + 10)}, Ptr: $codePtr")

    val opcode = code(codePtr) % 100
    val pm     = code(codePtr) / 100
    (opcode, pm % 10, pm / 10 % 10, pm / 100 % 10) match {
      case (1, m1, m2, _) =>
        // Addition
        val newValue = code(pos(code, codePtr + 1, m1)) + code(pos(code, codePtr + 2, m2))
        val writePos = code(codePtr + 3)
        (code.updated(writePos, newValue), Some(codePtr + 4), extra)
      case (2, m1, m2, _) =>
        // Multiplikation
        val newValue = code(pos(code, codePtr + 1, m1)) * code(pos(code, codePtr + 2, m2))
        val writePos = code(codePtr + 3)
        (code.updated(writePos, newValue), Some(codePtr + 4), extra)
      case (3, _, _, _) =>
        // Input
        val (in, rest) = extra("input").asInstanceOf[List[Int]].splitAt(1)
        val writePos   = code(codePtr + 1)
        (code.updated(writePos, in.head), Some(codePtr + 2), extra + ("input" -> rest))
      case (4, m1, _, _) =>
        // Output
        val readPos = pos(code, codePtr + 1, m1)
        val out     = code(readPos)
        println(s"Output: ${out}")
        (code, Some(codePtr + 2), extra + ("output" -> out))
      case (5, m1, m2, _) =>
        // Jump if TRUE
        val cp = if (code(pos(code, codePtr + 1, m1)) != 0) code(pos(code, codePtr + 2, m2)) else codePtr + 3
        (code, Some(cp), extra)
      case (6, m1, m2, _) =>
        // Jump if FALSE
        val cp = if (code(pos(code, codePtr + 1, m1)) == 0) code(pos(code, codePtr + 2, m2)) else codePtr + 3
        (code, Some(codePtr + 3), extra)
      case (7, m1, m2, _) =>
        // Less than
        val p1       = code(pos(code, codePtr + 1, m1))
        val p2       = code(pos(code, codePtr + 2, m2))
        val newValue = if (p1 < p2) 1 else 0
        val writePos = code(codePtr + 3)
        (code.updated(writePos, newValue), Some(codePtr + 4), extra)
      case (8, m1, m2, _) =>
        // Equals
        val p1       = code(pos(code, codePtr + 1, m1))
        val p2       = code(pos(code, codePtr + 2, m2))
        val newValue = if (p1 == p2) 1 else 0
        val writePos = code(codePtr + 3)
        (code.updated(writePos, newValue), Some(codePtr + 4), extra)
      case (99, _, _, _) =>
        // Terminate
        (code, None, extra)
      case _ =>
        println(s"Invalid opcode '$opcode' at pos: $codePtr'")
        throw new Exception(s"Invalid opcode '$opcode' at pos: $codePtr'")
    }
  }

  def pos(code: List[Int], cp: Int, mode: Int): Int = {
    if (mode == 0)
      code(cp)
    else if (mode == 1)
      cp
    else
      throw new Exception(s"Mode '$mode' is not valid, CP: $cp")
  }

  @tailrec
  def run(code: List[Int], codePtr: Int = 0, extra: Map[String, Any]): Int = {
    executeOne(code, codePtr, extra) match {
      case (newCode, Some(newPtr), extra) => run(newCode, newPtr, extra)
      case (finalCode, None, extra)       => extra("output").asInstanceOf[Int]
    }
  }

  val in     = readLine("Input: ")
  val result = run(input, extra = Map("input" -> List(in.toInt)))
  println(result)

  // Part two

}
