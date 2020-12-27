package se.joja

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Success, Try}

package object joja {

  def getInput(file: String): List[String] =
    Source.fromResource(file).getLines().toList

  def splitOnEmptyLine(in: List[String]): List[List[String]] =
    in.foldLeft(List(List.empty[String])) { (acc, line) =>
      if (line.isBlank) acc :+ List.empty else acc.init :+ (acc.last :+ line)
    }
      .filterNot(_.isEmpty)

  def groupInputOnStarting[A](input: List[A])(f: A => Boolean): List[List[A]] =
    input.foldLeft(List.empty[List[A]]) {
      case (acc, line) =>
        (f(line), acc) match {
          case (false, Nil) => acc :+ List(line)
          case (false, _)   => acc.init :+ (acc.last :+ line)
          case (true, _)    => acc :+ List(line)
        }
    }

  def invertMap[A, B](in: Map[A, List[B]]): Map[B, List[A]] = {
    val a = in.toList.flatMap { case (key, list) => list.map(_ -> key) }
    val b = a.groupBy(_._1)
    val c = b.mapValues(l => l.map(_._2))
    c.toMap
  }

  /**
   * greatest common divisor
   */
  @tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0) a.abs else gcd(b, a % b)

  /**
   * least common multiple
   */
  def lcm(a: Long, b: Long): Long = a * (b / gcd(a, b))
  def lcm(numbers: Seq[Long]): Long = numbers.reduce(lcm)

  /**
    * Chinese remainder theorem
    * from https://rosettacode.org/wiki/Chinese_remainder_theorem#Scala
    */
  def chineseRemainder(n: List[Long], a: List[Long]): Option[Long] = {
    require(n.size == a.size)
    val prod = n.product

    @tailrec
    def iter(n: List[Long], a: List[Long], sm: Long): Long = {
      def mulInv(a: Long, b: Long): Long = {
        @tailrec
        def loop(a: Long, b: Long, x0: Long, x1: Long): Long = {
          if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1
        }

        if (b == 1) 1
        else {
          val x1 = loop(a, b, 0, 1)
          if (x1 < 0) x1 + b else x1
        }
      }

      if (n.nonEmpty) {
        val p = prod / n.head

        iter(n.tail, a.tail, sm + a.head * mulInv(p, n.head) * p)
      } else sm
    }

    Try {
      iter(n, a, 0) % prod
    } match {
      case Success(v) => Some(v)
      case _          => None
    }
  }
}
