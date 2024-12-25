package se.joja.aoc.year2023

import se.joja.aoc.{readInput, splitOnEmptyLine}

import scala.annotation.tailrec

object Day19 extends App {

  val workflowsRaw :: partsRaw :: Nil = splitOnEmptyLine(readInput(2023, 19))

  case class Part(categories: Map[Char, Int], state: String = "in") {
    def isFinal: Boolean = state == "A" || state == "R"
    def rating: Int = categories.values.sum
  }

  case class Workflow(name: String, rules: List[Rule]) {
    def run(p: Part): Part = {
      val newState = rules.foldLeft(Option.empty[String]) { case (acc, r) =>
        acc match {
          case Some(_) => acc
          case None => r.run(p)
        }
      }
      p.copy(state = newState.get)
    }
  }

  sealed trait Rule {
    def run(p: Part): Option[String]
  }
  case class Condition(pred: Part => Boolean, dest: String) extends Rule {
    override def run(p: Part): Option[String] = if (pred(p)) Some(dest) else None
  }
  case class Fixed(dest: String) extends Rule {
    override def run(p: Part): Option[String] = Some(dest)
  }

  object Rule {
    def apply(s: String): Rule = {
      s match {
        case ConditionRegex(cat, op, value, dest) =>
          def pred(p: Part): Boolean = op match {
            case ">" => p.categories(cat.head) > value.toInt
            case "<" => p.categories(cat.head) < value.toInt
          }
          Condition(pred, dest)
        case dest => Fixed(dest)
      }
    }
  }

  val WorkflowRegex = """(\w+)\{(.*)}""".r
  val PartRegex = """\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}""".r
  val ConditionRegex = """(\w)([><])(\d+):(\w+)""".r

  val workflows = workflowsRaw.map {
    case WorkflowRegex(name, rulesRaw) =>
      val rules = rulesRaw.split(',').map(Rule(_)).toList
      name -> Workflow(name, rules)
  }.toMap

  val parts = partsRaw.map {
    case PartRegex(x, m, a, s) => Part(Map('x' -> x.toInt, 'm' -> m.toInt, 'a' -> a.toInt, 's' -> s.toInt))
  }

  @tailrec
  def runWorkflows(p: Part): Part = {
    if (p.isFinal) p else {
      runWorkflows(workflows(p.state).run(p))
    }
  }

  val finalParts = parts.map(runWorkflows).filter(_.state == "A").map(_.rating).sum

  val result1 = finalParts

  println(s"Result part 1: $result1")

  // Part 2

  val result2 = ""

  println(s"Result part 2: $result2")
}

