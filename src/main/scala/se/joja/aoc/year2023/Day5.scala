package se.joja.aoc.year2023

import se.joja.aoc.{getInput, splitOnEmptyLine}

import scala.annotation.tailrec

object Day5 extends App {

  val input = splitOnEmptyLine(getInput("2023/day5.txt"))
  val seedsRaw :: mapsRaw = input

  val seeds = seedsRaw.head.split(':').last.split(' ').filter(_.nonEmpty).map(_.toLong).toList

  case class Mapping(from: Long, to: Long, diff: Long) {
    def doMap(in: Long): Option[Long] =
      if (isIn(in)) Some(in + diff) else None

    def isIn(in: Long): Boolean = in >= from && in <= to

    def isOverlapping(other: Mapping): Boolean = {
      val first :: second :: _ = List(this, other).sortBy(_.from)
      first.to >= second.from
    }

    def canMerge(other: Mapping): Boolean =
      (to + 1 == other.from || other.to + 1 == from) && diff == other.diff

    def combine(other: Mapping): List[Mapping] = {
      println(s"Combining $this with $other")
      val first :: second :: _ = List(this, other).sortBy(_.from)

      val allMappings = (first.from, first.to + 1, second.from, second.to + 1) match {
        case (f1, f2, s1, s2) if f2 < s1 => // Not touching
          List(first, second)
        case (f1, f2, s1, s2) if f2 == s1 && first.diff == second.diff => // Touching and same diff
          List(first.copy(to = s2 - 1))
        case (f1, f2, s1, s2) if f2 >= s1 && f2 <= s2 => // Overlapping partial
          List(first.copy(to = s1 - 1), Mapping(s1, f2 - 1, first.diff + second.diff), second.copy(from = f2))
        case (f1, f2, s1, s2) if f2 >= s1 && f2 >= s2 => // Overlapping fully
          List(first.copy(to = s1 - 1), second.copy(diff = first.diff + second.diff), first.copy(from = s2))
      }

      allMappings.filter(m => m.from <= m.to && diff != 0)
    }
  }

  case class Converter(fromType: String, toType: String, mappings: List[Mapping]) {
    def doMap(in: Long): Long =
      mappings.find(_.isIn(in)).flatMap(_.doMap(in)).getOrElse(in)
  }

  val MappingHeadRegex = """(\w+)-to-(\w+) map:""".r
  val MappingRowRegex = """(\d+) (\d+) (\d+)""".r

  val converters = mapsRaw.map { convRaw =>
    val convHeadRaw :: mappingsRaw = convRaw

    val mappings = mappingsRaw.map {
      case MappingRowRegex(to, from, size) => Mapping(from.toLong, from.toLong + size.toLong - 1, to.toLong - from.toLong)
    }

    convHeadRaw match {
      case MappingHeadRegex(from, to) => Converter(from, to, mappings)
    }
  }

  val mappedSeeds = converters.foldLeft(seeds) { case (acc, conv) =>
    acc.map(conv.doMap)
  }

  val result1 = mappedSeeds.min
  println(s"Result part 1: ${result1}")

  // Part 2

  @tailrec
  def fixOverlapping(curr: List[Mapping]): List[Mapping] = {
    if (curr.length < 2) curr else {
      val sorted = curr.sortBy(_.from)
      val maybeOverlap = sorted.sliding(2).toList.find { case m1 :: m2 :: _ =>
        m1.isOverlapping(m2) || m1.canMerge(m2)
      }
      maybeOverlap match {
        case Some(m1 :: m2 :: _) =>
          fixOverlapping(sorted.diff(List(m1, m2)) ++ m1.combine(m2))
        case None => sorted
      }
    }
  }

  val combinedMappings = converters.flatMap(_.mappings).foldLeft(List.empty[Mapping]) { case (acc, m) =>
    println(s"Handling mapping $m, acc is $acc")
    fixOverlapping(acc :+ m)
  }

  val result2 = seeds.flatMap(s => combinedMappings.find(_.isIn(s)).map(m => s + m.diff))
  println(s"Result part 2: ${result2}")
}

