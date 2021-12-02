package se.joja.aoc.year2019

import se.joja.aoc.getInput

import java.util.concurrent.{Executors, LinkedTransferQueue}
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

object Day9 extends App {
  implicit val ec: ExecutionContextExecutorService =
    ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(8))

  val code = getInput("2019/day9.txt")(0).split(",").map(_.toLong).toList

  type Que        = LinkedTransferQueue[Long]
  type CodeAndMem = (List[Long], Map[Long, Long])

  def executeOne(codeAndMem: CodeAndMem,
                 codePtr: Int,
                 inQ: Que,
                 outQ: Que,
                 extra: Map[String, Any] = Map.empty): (CodeAndMem, Option[Int], Map[String, Any]) = {
    val (code, mem) = codeAndMem

    val relBase = extra.getOrElse("relBase", 0L).asInstanceOf[Long]

    //println(s"Code: ${code.slice(codePtr, codePtr + 10)}, Ptr: $codePtr, RelBase: $relBase")

    def pos(cp: Int, mode: Long): Long =
      mode match {
        case 0 => read(cp)            // Position mode
        case 1 => cp                  // Immediate mode
        case 2 => relBase + read(cp)  // Relative mode
        case _ => throw new Exception(s"Mode '$mode' is not valid, CP: $cp")
      }

    def read(addr: Long): Long =
      addr match {
        case e if e < 0 => throw new Exception(s"Tried to read memory at illegal pos $e")
        case a if a >= code.size =>
          val v = mem.getOrElse(addr, 0L)
          //println(s"Reading mem at $addr = $v")
          v
        case a => code(addr.toInt)
      }

    def write(addr: Long, value: Long): CodeAndMem =
      addr match {
        case e if e < 0 => throw new Exception(s"Tried to write memory at illegal pos $addr")
        case a if a >= code.size =>
          val v = (code, mem.updated(addr, value))
          println(s"Writing MEM at $addr = $value")
          v
        case a =>
          val v = (code.updated(addr.toInt, value), mem)
          //println(s"Writing CODE at $addr = $value")
          v
      }

    val opcode = read(codePtr) % 100
    val pm     = read(codePtr) / 100
    (opcode, pm % 10, pm / 10 % 10, pm / 100 % 10) match {
      case (1, m1, m2, m3) =>
        // Addition
        val newValue = read(pos(codePtr + 1, m1)) + read(pos(codePtr + 2, m2))
        val writePos = pos(codePtr + 3, m3)
        (write(writePos, newValue), Some(codePtr + 4), extra)
      case (2, m1, m2, m3) =>
        // Multiplikation
        val newValue = read(pos(codePtr + 1, m1)) * read(pos(codePtr + 2, m2))
        val writePos = pos(codePtr + 3, m3)
        (write(writePos, newValue), Some(codePtr + 4), extra)
      case (3, m1, _, _) =>
        // Input
        val writePos = pos(codePtr + 1, m1)
        val r        = inQ.take()
        println(s"Read input(${Thread.currentThread().getId}): $r")
        (write(writePos, r), Some(codePtr + 2), extra)
      case (4, m1, _, _) =>
        // Output
        val readPos = pos(codePtr + 1, m1)
        val out     = read(readPos)
        println(s"Output(${Thread.currentThread().getId}): $out")
        outQ.put(out)
        (codeAndMem, Some(codePtr + 2), extra)
      case (5, m1, m2, _) =>
        // Jump if TRUE
        val cp = if (read(pos(codePtr + 1, m1)) != 0) read(pos(codePtr + 2, m2)) else codePtr + 3
        (codeAndMem, Some(cp.toInt), extra)
      case (6, m1, m2, _) =>
        // Jump if FALSE
        val cp = if (read(pos(codePtr + 1, m1)) == 0) read(pos(codePtr + 2, m2)) else codePtr + 3
        (codeAndMem, Some(cp.toInt), extra)
      case (7, m1, m2, m3) =>
        // Less than
        val newValue = if (read(pos(codePtr + 1, m1)) < read(pos(codePtr + 2, m2))) 1 else 0
        val writePos = pos(codePtr + 3, m3)
        (write(writePos, newValue), Some(codePtr + 4), extra)
      case (8, m1, m2, m3) =>
        // Equals
        val newValue = if (read(pos(codePtr + 1, m1)) == read(pos(codePtr + 2, m2))) 1 else 0
        val writePos = pos(codePtr + 3, m3)
        (write(writePos, newValue), Some(codePtr + 4), extra)
      case (9, m1, _, _) =>
        // Relative base adjust
        val newRelBase = relBase + read(pos(codePtr + 1, m1))
        (codeAndMem, Some(codePtr + 2), extra + ("relBase" -> newRelBase))
      case (99, _, _, _) =>
        // Terminate
        println(s"Terminated(${Thread.currentThread().getId})")
        (codeAndMem, None, extra)
      case _ =>
        println(s"Invalid opcode '$opcode' at pos: $codePtr'")
        throw new Exception(s"Invalid opcode '$opcode' at pos: $codePtr'")
    }
  }
  @tailrec
  def run(codeAndMem: CodeAndMem,
          codePtr: Int = 0,
          inQ: Que = new Que(),
          outQ: Que = new Que(),
          extra: Map[String, Any] = Map.empty): Option[Long] = {
    executeOne(codeAndMem, codePtr, inQ, outQ, extra) match {
      case (newCodeAndMem, Some(newPtr), extra) => run(newCodeAndMem, newPtr, inQ, outQ, extra)
      case (_, None, _)                         => if (outQ.isEmpty) None else Some(outQ.take())
    }
  }

  val input = new Que()
  input.add(1)

  val res = run((code, Map.empty), inQ = input).get

  println(s"Result: $res")

  // Part two

  val input2 = new Que()
  input2.add(2)

  val res2 = run((code, Map.empty), inQ = input2).get

  println(s"Result2: $res2")
}
