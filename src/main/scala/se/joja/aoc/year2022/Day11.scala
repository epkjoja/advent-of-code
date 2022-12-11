package se.joja.aoc.year2022

import se.joja.aoc.{getInput, splitOnEmptyLine}

import scala.collection.mutable

object Day11 extends App {

  val input       = getInput("2022/day11.txt")
  val inputBlocks = splitOnEmptyLine(input)

  val InMonkey    = """Monkey (\d+):""".r
  val InStarting  = """\s+Starting items: ([\d, ]+)""".r
  val InOperation = """\s+Operation: new = old ([-+*/]) (\d+|old)""".r
  val InTest      = """\s+Test: divisible by (\d+)""".r
  val InTrueFalse = """\s+If (true|false): throw to monkey (\d+)""".r

  case class Monkey(
      id: Int = 0,
      items: List[Long] = List.empty,
      operation: Long => Long = identity,
      test: Int = 0,
      ifTrue: Int = 0,
      ifFalse: Int = 0,
      numInspections: Long = 0
  )

  def createOperation(op: String, constant: Option[Int]): Long => Long =
    (op, constant) match {
      case ("+", Some(c)) => _ + c
      case ("+", None)    => _ * 2
      case ("-", Some(c)) => _ - c
      case ("-", None) =>
        _ =>
          0
      case ("*", Some(c)) => _ * c
      case ("*", None) =>
        i =>
          i * i
      case ("/", Some(c)) => _ / c
      case ("/", None)    => ???
    }

  val monkeys = inputBlocks.map { block =>
    block.foldLeft(Monkey()) { (m, line) =>
      line match {
        case InMonkey(id)                => m.copy(id = id.toInt)
        case InStarting(items)           => m.copy(items = items.trim.split(", ").toList.map(_.toLong))
        case InOperation(op, "old")      => m.copy(operation = createOperation(op, None))
        case InOperation(op, const)      => m.copy(operation = createOperation(op, Some(const.toInt)))
        case InTest(value)               => m.copy(test = value.toInt)
        case InTrueFalse("true", value)  => m.copy(ifTrue = value.toInt)
        case InTrueFalse("false", value) => m.copy(ifFalse = value.toInt)
      }
    }
  }

  println(monkeys.mkString("\n"))

  val allPeriod = monkeys.map(_.test).product
  println(s"All tests product: $allPeriod")

  def doRound(monkeys: List[Monkey], worryOp: Long => Long): List[Monkey] = {
    val items = monkeys.map(m => m.id -> m.items).toMap.to(mutable.Map)
    val numInsp = monkeys.map(m => m.id -> m.numInspections).toMap.to(mutable.Map)

    monkeys.foreach { m =>
      items(m.id).foreach { item =>
        val afterOp = m.operation(item)
        val afterBored = worryOp(afterOp)
        afterBored % m.test match {
          case 0 => items(m.ifTrue) = items(m.ifTrue) :+ (afterBored % allPeriod)
          case _ => items(m.ifFalse) = items(m.ifFalse) :+ (afterBored % allPeriod)
        }
        numInsp(m.id) += 1
      }
      items(m.id) = List.empty
    }

    monkeys.map(m => m.copy(items = items(m.id), numInspections = numInsp(m.id)))
  }

  val afterRound20 = 0.until(20).foldLeft(monkeys) { (acc, _) =>
    doRound(acc, _ / 3)
  }

  println(afterRound20.mkString("\n"))

  val result1 = afterRound20.map(_.numInspections).sorted.reverse.take(2).product
  println(s"Result part 1: $result1")

  // Part 2

  val afterRound10k = 0.until(10_000).foldLeft(monkeys) { (acc, i) =>
    //println(s"Starting round $i")
    doRound(acc, identity)
  }

  println(afterRound10k.mkString("\n"))

  val result2 = afterRound10k.map(_.numInspections).sorted.reverse.take(2).product
  println(s"Result part 2: $result2")
}
