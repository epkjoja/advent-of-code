package se.joja.aoc.year2021

import se.joja.aoc.getInput

object Day18 extends App {

  val input = getInput("2021/day18.txt").map(Num.parse(_)._1)
  println(input)

  // Part 1

  sealed trait Num {
    def add(n: Num): Num = Pair(this, n).reduce
    def reduce: Num = {
      println(s"Reducing: $this")
      this.tryExplode() match {
        case (explodedNum, true, _, _) =>
          println(s"Explo to: $explodedNum")
          explodedNum.reduce
        case (_, false, _, _) =>
          this.trySplit match {
            case (splitNum, true) =>
              println(s"Split to: $splitNum")
              splitNum.reduce
            case (_, false) => this
          }
      }
    }

    def tryExplode(depth: Int = 1): (Num, Boolean, Option[Int], Option[Int])
    def trySplit: (Num, Boolean)
    def addToFirstSingle(i: Int): Num
    def addToLastSingle(i: Int): Num
    def magnitude: Long
  }
  object Num {
    def parse(s: String): (Num, String) =
      s.head match {
        case '[' => Pair.parse(s)
        case _   => Single.parse(s)
      }
  }

  case class Pair(a: Num, b: Num) extends Num {
    override def tryExplode(depth: Int): (Num, Boolean, Option[Int], Option[Int]) = {
      (depth, a, b) match {
        case (4, Pair(Single(s1), Single(s2)), b) =>
          (Pair(Single(0), b.addToFirstSingle(s2)), true, Some(s1), None)
        case (4, Single(s), Pair(Single(s1), Single(s2))) =>
          (Pair(Single(s + s1), Single(0)), true, None, Some(s2))
        case (d, a, b) =>
          a.tryExplode(d + 1) match {
            case (numA, true, mayA, mayB) =>
              val newB = if (mayB.isDefined) b.addToFirstSingle(mayB.get) else b
              (Pair(numA, newB), true, mayA, None)
            case (numA, false, _, _) =>
              b.tryExplode(d + 1) match {
                case (numB, true, Some(restA), mayB) =>
                  (Pair(a.addToLastSingle(restA), numB), true, None, mayB)
                case (numB, exploded, mayA, mayB) =>
                  (Pair(numA, numB), exploded, mayA, mayB)
              }
          }
      }
    }

    override def trySplit: (Num, Boolean) = {
      a.trySplit match {
        case (newA, true) => (Pair(newA, b), true)
        case (_, false) =>
          b.trySplit match {
            case (newB, true) => (Pair(a, newB), true)
            case (_, false)   => (this, false)
          }
      }
    }

    override def addToFirstSingle(i: Int): Num =
      Pair(a.addToFirstSingle(i), b)

    override def addToLastSingle(i: Int): Num =
      Pair(a, b.addToLastSingle(i))

    override def magnitude: Long =
      3 * a.magnitude + 2 * b.magnitude

    override def toString: String = s"[$a,$b]"
  }
  object Pair {
    def parse(s: String): (Num, String) = {
      if (s.head != '[') throw new RuntimeException(s"Unexpected [ in $s")
      val (p1, rest1) = Num.parse(s.tail)
      if (rest1.head != ',') throw new RuntimeException(s"Unexpected , in $s")
      val (p2, rest2) = Num.parse(rest1.tail)
      if (rest2.head != ']') throw new RuntimeException(s"Unexpected ] in $s")
      Pair(p1, p2) -> rest2.tail
    }
  }

  case class Single(a: Int) extends Num {
    override def addToFirstSingle(i: Int): Num = Single(a + i)
    override def addToLastSingle(i: Int): Num = Single(a + i)

    override def tryExplode(depth: Int): (Num, Boolean, Option[Int], Option[Int]) =
      (this, false, None, None)

    override def trySplit: (Num, Boolean) = {
      if (a < 10) (this, false)
      else {
        val div = a / 2
        (Pair(Single(div), Single(a - div)), true)
      }
    }

    override def magnitude: Long = a

    override def toString: String = a.toString
  }
  object Single {
    def parse(s: String): (Num, String) = {
      val (d, rest) = s.span(_.isDigit)
      Single(d.toInt) -> rest
    }
  }

  val finalNum = input.tail.foldLeft(input.head) {
    case (acc, n) =>
      println(s"  $acc")
      println(s"+ $n")
      val sum = acc.add(n)
      println(s"= $sum\n")
      sum
  }
  println(finalNum)
  println(s"Result part 1: ${finalNum.magnitude}")

  // Part 2

  val allPairs = for {
    i <- 0.until(input.size - 1)
    j <- (i + 1).until(input.size)
  } yield (input(i), input(j))

  val sums = allPairs.flatMap {
    case (n1, n2) =>
      List(n1.add(n2).magnitude, n2.add(n1).magnitude)
  }.toList
  println(sums)

  val result2 = sums.max
  println(s"Result part 2: $result2")

}
