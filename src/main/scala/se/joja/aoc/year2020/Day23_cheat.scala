package se.joja.aoc.year2020

object Day23_cheat {

  implicit class LazyListUnfoldOps(lazyList: LazyList.type) {
    // https://github.com/tpolecat/examples/blob/ab444af9101b9049d6bd7ebf13ae583bc77ac60a/src/main/scala/eg/Unfold.scala
    // converted to Scala 2.13 LazyList, unfold now standard (but with return arguments swapped)
    def unfold0[A](a: A)(f: A => Option[A]): LazyList[A] =
      LazyList.unfold(a)(a => f(a).map(a => (a, a)))
  }

  implicit class IndexIteratorOps[A](it: Iterator[A]) {
    def headOption: Option[A] = if (it.nonEmpty) Some(it.next()) else None
    def head: A = headOption.get

    def lastOption: Option[A] = it.reduceOption((_, x) => x)
    def last: A = lastOption.get

    def apply(i: Int): A = it.drop(i).head
  }

  type Cups = LazyList[Int]
  type NextMap = collection.Seq[Int]

  def nextMap2cups(nextMap: NextMap, after: Int): Cups = {
    // excludes after
    LazyList.unfold0(after)(i => Some(nextMap(i))).takeWhile(_ != after)
  }

  def simulateMoves(cups: Cups, moves: Int = 100): NextMap = {
    val max = cups.max
    // primitive array because mutable.ArraySeq has boxing overhead
    val nextMap = Array.ofDim[Int](1 + max) // 0 is unused
    for ((i, next) <- cups.lazyZip(cups.tail))
      nextMap(i) = next
    nextMap(cups.last) = cups.head

    // TODO: inline Iterator.iterate-s to avoid boxing

    def helper(current: Int): Int = {
      val pick1 = nextMap(current)
      val pick2 = nextMap(pick1)
      val pick3 = nextMap(pick2)

      val destination =
      // inlined %+ to avoid boxing
        Iterator.iterate(current)(i => (i - 1 - 1 + max) % max + 1)
          .drop(1)
          .find(i => i != pick1 && i != pick2 && i != pick3)
          .get

      nextMap(current) = nextMap(pick3)
      nextMap(pick3) = nextMap(destination)
      nextMap(destination) = pick1

      nextMap(current)
    }

    Iterator.iterate(cups.head)(helper)(moves)
    nextMap
  }

  def simulateMovesLabels(cups: Cups, moves: Int = 100): String = {
    val finalNextMap = simulateMoves(cups, moves)
    nextMap2cups(finalNextMap, 1).mkString("")
  }

  def simulateMovesLabelsPart2(cups: Cups, moves: Int = 10000000): Long = {
    val newCups = cups ++ ((cups.max + 1) to 1000000)
    val finalNextMap = simulateMoves(newCups, moves)
    nextMap2cups(finalNextMap, 1)
      .take(2)
      .map(_.toLong)
      .product
  }


  def parseCups(input: String): Cups = input.map(_.asDigit).to(LazyList)

  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim
  val input: String = "716892543"

  def main(args: Array[String]): Unit = {
    println(simulateMovesLabels(parseCups(input)))
    println(simulateMovesLabelsPart2(parseCups(input)))
  }
}
