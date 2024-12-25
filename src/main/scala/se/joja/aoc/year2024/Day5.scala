package se.joja.aoc.year2024

import se.joja.aoc.{invertMap, readInput, splitOnEmptyLine}

object Day5 extends App {

  val orderRulesRaw :: pagesRaw :: Nil = splitOnEmptyLine(readInput(2024, 5, true))

  val orderRules = orderRulesRaw.map(_.split('|')).groupMapReduce(_.last.toInt)(_.init.map(_.toInt).toSet)(_ ++ _)
  val updates = pagesRaw.map(_.split(',').map(_.toInt).toList)

  println(orderRules.mkString("\n"))
  println(updates.mkString("\n"))

  val checkedUpdates = updates.map { update =>
    val (_, valid) = update.foldLeft((Set.empty[Int], true)) { case ((notAllowed, valid), page) =>
      if (!valid || notAllowed.contains(page))
        (notAllowed, false)
      else
        (notAllowed ++ orderRules.getOrElse(page, Set.empty), true)
    }

    (valid, update)
  }

  val validMiddles = checkedUpdates.filter(_._1).map(p => p._2(p._2.size / 2))
  val result1 = validMiddles.sum

  println(s"Result part 1: $result1")

  // Part 2

  val invalidUpdates = checkedUpdates.filterNot(_._1).map(_._2)
  val orderRules2 = invertMap(orderRules.map(r => r._1 -> r._2.toList))



  val result2 = ""

  println(s"Result part 2: $result2")
}

