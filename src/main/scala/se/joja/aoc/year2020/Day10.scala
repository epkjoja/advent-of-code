package se.joja.aoc.year2020

import se.joja.aoc.getInput

import scala.collection.mutable

object Day10 extends App {

  val input = getInput("2020/day10.txt").map(_.toInt).sorted

  val hops = input.sliding(2).map(x => x.head -> x.last).foldLeft((0, 0)) {
    case ((numOne, numThree), (old, curr)) =>
      curr - old match {
        case 1 => numOne + 1 -> numThree
        case 3 => numOne     -> (numThree + 1)
        case _ => numOne     -> numThree
      }
  }

  val result = (hops._1 + 1) * (hops._2 + 1)
  println(s"Result: $result, $hops")

  val alt2 = (List(0) ++ input :+ (input.max + 3)).sliding(2).toList.map(p => p.last - p.head)
  val res_alt2 = alt2.count(_ == 1) * alt2.count(_ == 3)
  println(s"Result (alt2): $res_alt2, $alt2")

  // Part two

  val cache = mutable.Map.empty[Int, Long]

  def count(plugs: List[Int], current: Int = 0): Long = {
    if (cache.contains(current)) cache(current) else {
      val ans = plugs.takeWhile(p => p - current <= 3).size match {
        case 0 => 1
        case 1 => count(plugs.drop(1), plugs.head)
        case 2 => count(plugs.drop(1), plugs.head) + count(plugs.drop(2), plugs(1))
        case 3 => count(plugs.drop(1), plugs.head) + count(plugs.drop(2), plugs(1)) + count(plugs.drop(3), plugs(2))
      }
      cache.put(current, ans)
      ans
    }
  }

  println(s"Result2: ${count(input)}")
}
