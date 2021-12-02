package se.joja.aoc.year2019

import se.joja.aoc.getInput

import java.util.concurrent.{Executors, LinkedTransferQueue}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

object Day7 extends App {
  implicit val ec = ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(8))

  val code = getInput("2019/day7.txt").head.split(",").map(_.toLong).toList

  type Que = LinkedTransferQueue[Long]

  def executeOne(code: List[Long],
                 codePtr: Int,
                 inQ: Que,
                 outQ: Que): (List[Long], Option[Int]) = {
    //println(s"Code: ${code.slice(codePtr, codePtr + 10)}, Ptr: $codePtr")

    val opcode = code(codePtr) % 100
    val pm     = code(codePtr) / 100
    (opcode, pm % 10, pm / 10 % 10, pm / 100 % 10) match {
      case (1, m1, m2, _) =>
        // Addition
        val newValue = code(pos(code, codePtr + 1, m1).toInt) + code(pos(code, codePtr + 2, m2).toInt)
        val writePos = code(codePtr + 3).toInt
        (code.updated(writePos, newValue), Some(codePtr + 4))
      case (2, m1, m2, _) =>
        // Multiplikation
        val newValue = code(pos(code, codePtr + 1, m1).toInt) * code(pos(code, codePtr + 2, m2).toInt)
        val writePos = code(codePtr + 3).toInt
        (code.updated(writePos, newValue), Some(codePtr + 4))
      case (3, _, _, _) =>
        // Input
        val writePos   = code(codePtr + 1).toInt
        val read = inQ.take().toInt
        println(s"Read input(${Thread.currentThread().getId}): $read")
        (code.updated(writePos, read), Some(codePtr + 2))
      case (4, m1, _, _) =>
        // Output
        val readPos = pos(code, codePtr + 1, m1).toInt
        val out     = code(readPos)
        println(s"Output(${Thread.currentThread().getId}): $out")
        outQ.put(out)
        (code, Some(codePtr + 2))
      case (5, m1, m2, _) =>
        // Jump if TRUE
        val cp = if (code(pos(code, codePtr + 1, m1).toInt) != 0) code(pos(code, codePtr + 2, m2).toInt) else codePtr + 3
        (code, Some(cp.toInt))
      case (6, m1, m2, _) =>
        // Jump if FALSE
        val cp = if (code(pos(code, codePtr + 1, m1).toInt) == 0) code(pos(code, codePtr + 2, m2).toInt) else codePtr + 3
        (code, Some(codePtr + 3))
      case (7, m1, m2, _) =>
        // Less than
        val p1       = code(pos(code, codePtr + 1, m1).toInt)
        val p2       = code(pos(code, codePtr + 2, m2).toInt)
        val newValue = if (p1 < p2) 1 else 0
        val writePos = code(codePtr + 3)
        (code.updated(writePos.toInt, newValue), Some(codePtr + 4))
      case (8, m1, m2, _) =>
        // Equals
        val p1       = code(pos(code, codePtr + 1, m1).toInt)
        val p2       = code(pos(code, codePtr + 2, m2).toInt)
        val newValue = if (p1 == p2) 1 else 0
        val writePos = code(codePtr + 3).toInt
        (code.updated(writePos, newValue), Some(codePtr + 4))
      case (99, _, _, _) =>
        // Terminate
        println(s"Terminated(${Thread.currentThread().getId})")
        (code, None)
      case _ =>
        println(s"Invalid opcode '$opcode' at pos: $codePtr'")
        throw new Exception(s"Invalid opcode '$opcode' at pos: $codePtr'")
    }
  }

  def pos(code: List[Long], cp: Int, mode: Long): Long =
    mode match {
      case 0 => code(cp)
      case 1 => cp
      case _ => throw new Exception(s"Mode '$mode' is not valid, CP: $cp")
    }

  @tailrec
  def run(code: List[Long], codePtr: Int = 0, inQ: Que, outQ: Que): Option[Long] = {
    executeOne(code, codePtr, inQ, outQ) match {
      case (newCode, Some(newPtr)) => run(newCode, newPtr, inQ, outQ)
      case (_, None)               => if (outQ.isEmpty) None else Some(outQ.take())
    }
  }

  val perms = 0.to(4).toList.permutations.toList

  val cache = mutable.Map.empty[(Int, Long), Long]

  @tailrec
  def calcOnePerm(perm: List[Int], in: Long = 0): Long = {
    perm match {
      case phase :: rest if cache.contains((phase, in)) =>
        calcOnePerm(rest, cache((phase, in)))
      case phase :: rest =>
        val inQ = new Que()
        inQ.put(phase)
        inQ.put(in)
        val outQ = new Que()
        val out = run(code, inQ = inQ, outQ = outQ).get
        cache.put((phase, in), out)
        calcOnePerm(rest, out)
      case Nil => in
    }
  }

  //val allOut = perms.map { perm =>
  //  calcOnePerm(perm) -> perm.mkString(",")
  //}.toMap

  //val res = allOut.maxBy(_._1)
  //println(s"Result: $res")

  // Part two


  def startAmp(num: Int, code: List[Long], inQ: Que, outQ: Que): Future[Option[Long]] = {
    Future {
      println(s"Amp $num started(${Thread.currentThread().getId})")
      val res = run(code, inQ = inQ, outQ = outQ)
      println(s"Amp $num stopping(${Thread.currentThread().getId})")
      res
    }
  }

  def testOnePerm(perm: List[Int]): Future[Long] = {
    val qs = perm.map { phase =>
      val q = new Que()
      q.add(phase.toLong)
      q
    }

    val a1 = startAmp(1, code, qs(0), qs(1))
    val a2 = startAmp(2, code, qs(1), qs(2))
    val a3 = startAmp(3, code, qs(2), qs(3))
    val a4 = startAmp(4, code, qs(3), qs(4))
    val a5 = startAmp(5, code, qs(4), qs(0))

    val fin = for {
     a1r <- a1
     a2r <- a2
     a3r <- a3
     a4r <- a4
     a5r <- a5
    } yield a5r.get

    qs.head.add(0)

    fin
  }

  val perms2 = 5.to(9).toList.permutations.toList

  val allOut = perms2.map { perm =>
    val out = Await.result(testOnePerm(perm), 10.seconds)
    (out, perm.mkString("."))
  }

  val res = allOut.maxBy(_._1)

  println(s"Result2: $res")
}
