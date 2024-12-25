package se.joja.aoc.year2024

import se.joja.aoc.readInput

import scala.collection.mutable

object Day24 extends App {

  val InputPattern = """(\w+): ([01])""".r
  val GatePattern = """(\w+) (AND|OR|XOR) (\w+) -> (\w+)""".r

  sealed abstract class Gate(val out: String) extends Product with Serializable
  case class And(override val out: String, in1: String, in2: String) extends Gate(out)
  case class Or(override val out: String, in1: String, in2: String) extends Gate(out)
  case class Xor(override val out: String, in1: String, in2: String) extends Gate(out)
  case class In(override val out: String, state: Boolean) extends Gate(out)

  val input = readInput(2024, 24).collect {
    case GatePattern(in1, "AND", in2, out) => And(out, in1, in2)
    case GatePattern(in1, "OR", in2, out) => Or(out, in1, in2)
    case GatePattern(in1, "XOR", in2, out) => Xor(out, in1, in2)
    case InputPattern(a, b) => In(a, b == "1")
  }

  val savedStates = mutable.Map.empty[String, Boolean]

  def calcState(s: String, gates: List[Gate]): Boolean = {
    if (savedStates.contains(s)) savedStates(s) else {
      val state = gates.find(_.out == s) match {
        case None => throw new RuntimeException("JOJA")
        case Some(In(_, state)) =>
          state
        case Some(And(_, in1, in2)) =>
          calcState(in1, gates) && calcState(in2, gates)
        case Some(Or(_, in1, in2)) =>
          calcState(in1, gates) || calcState(in2, gates)
        case Some(Xor(_, in1, in2)) =>
          calcState(in1, gates) ^ calcState(in2, gates)
      }
      savedStates(s) = state
      state
    }
  }

  val zStates = input
    .filter(_.out.head == 'z')
    .map(g => g.out -> (if (calcState(g.out, input)) 1 else 0))
    .sortBy(_._1)
    .reverse
    .map(_._2)
    .mkString

  val result1 = java.lang.Long.parseLong(zStates, 2)

  println(s"Result part 1: $result1")

  // Part 2

  val result2 = ""

  println(s"Result part 2: $result2")
}

