package se.joja.aoc.year2020

import se.joja.aoc.getInput

import scala.annotation.tailrec

object Day8 extends App {

  val input = getInput("2020/day8.txt")

  val regex = """(\w+) ([+-]\d+)""".r

  final val TERMINATED = "term"
  final val ERROR      = "error"

  def executeOne(codePtr: Int,
                 acc: Long,
                 visited: Set[Int],
                 code: List[String]): (Either[String, Int], Long, Set[Int]) = {
    if (codePtr >= code.size) {
      println(s"After last instr '$codePtr'")
      (Left(TERMINATED), acc, visited)
    } else if (visited.contains(codePtr)) {
      println(s"Already visited '$codePtr'")
      (Left(ERROR), acc, visited)
    } else {
      val (next, newAcc) = code(codePtr) match {
        case regex("nop", _)    => (codePtr + 1, acc)
        case regex("acc", data) => (codePtr + 1, acc + data.toInt)
        case regex("jmp", data) => (codePtr + data.toInt, acc)
      }
      (Right(next), newAcc, visited + codePtr)
    }
  }

  @tailrec
  def run(codePtr: Int = 0, acc: Long = 0, visited: Set[Int] = Set.empty, code: List[String]): (String, Long) = {
    executeOne(codePtr, acc, visited, code) match {
      case (Right(newPtr), newAcc, newVisited) => run(newPtr, newAcc, newVisited, code)
      case (Left(error), finalAcc, _)          => (error, finalAcc)
    }
  }

  println(run(code = input))

  // Part Two

  val allNopAndJmp = input.zipWithIndex.filter(i => Set("nop", "jmp").contains(i._1.take(3))).map(_._2).sorted

  def replaceOne(code: List[String], ptr: Int): List[String] = {
    code(ptr) match {
      case regex("nop", data) => code.updated(ptr, s"jmp $data")
      case regex("jmp", data) => code.updated(ptr, s"nop $data")
      case _                  => code
    }
  }

  val allInputs = allNopAndJmp.map(replace => replaceOne(input, replace) -> replace)

  val result2 = allInputs.map(input => run(code = input._1) -> input._2).filter(_._1._1 == TERMINATED)

  println(s"Result2: $result2")
}
