package se.joja.aoc.year2020

import se.joja.joja.getInput
import org.parboiled.scala._
import org.parboiled.errors.{ErrorUtils, ParsingException}

object Day18 extends App {

  val input = getInput("2020/day18.txt")

  val calc = new SimpleCalculatorOne

  val allRes = input.map { expr =>
    calc.calculate(expr)
  }

  println(s"Result: ${allRes.sum}")

  // Part Twq

  val calc2 = new SimpleCalculatorTwo

  val allRes2 = input.map(expr => calc2.calculate(expr))

  println(s"Result2: ${allRes2.sum}")

  /**
    * A parser for a simple calculator language supporting the 4 basic calculation types on integers.
    * The actual calculations are performed by inline parser actions using the parsers value stack as temporary storage.
    */
  class SimpleCalculatorOne extends Parser {

    def InputLine = rule { Expression ~ EOI }

    def Expression: Rule1[Long] = rule {
      Factor ~ zeroOrMore(
        " + " ~ Factor ~~> ((a: Long, b) => a + b)
          | " - " ~ Factor ~~> ((a: Long, b) => a - b)
          | " * " ~ Factor ~~> ((a: Long, b) => a * b)
          | " / " ~ Factor ~~> ((a: Long, b) => a / b)
      )
    }

    def Factor = rule { Number | Parens }

    def Parens = rule { "(" ~ Expression ~ ")" }

    def Number = rule { Digits ~> (_.toLong) }

    def Digits = rule { oneOrMore(Digit) }

    def Digit = rule { "0" - "9" }

    /**
      * The main parsing method. Uses a ReportingParseRunner (which only reports the first error) for simplicity.
      */
    def calculate(expression: String): Long = {
      val parsingResult = ReportingParseRunner(InputLine).run(expression)
      parsingResult.result match {
        case Some(i) => i
        case None =>
          throw new ParsingException(
            "Invalid calculation expression:\n" +
              ErrorUtils.printParseErrors(parsingResult))
      }
    }
  }

  /**
    * A parser for a simple calculator language supporting the 4 basic calculation types on integers.
    * The actual calculations are performed by inline parser actions using the parsers value stack as temporary storage.
    */
  class SimpleCalculatorTwo extends Parser {

    def InputLine = rule { Expression ~ EOI }

    def Expression: Rule1[Long] = rule {
      Term ~ zeroOrMore(
        " * " ~ Term ~~> ((a: Long, b) => a * b)
          | " / " ~ Term ~~> ((a: Long, b) => a / b)
      )
    }

    def Term = rule {
      Factor ~ zeroOrMore(
        " + " ~ Factor ~~> ((a: Long, b) => a + b)
          | " - " ~ Factor ~~> ((a: Long, b) => a - b)
      )
    }

    def Factor = rule { Number | Parens }

    def Parens = rule { "(" ~ Expression ~ ")" }

    def Number = rule { Digits ~> (_.toLong) }

    def Digits = rule { oneOrMore(Digit) }

    def Digit = rule { "0" - "9" }

    /**
      * The main parsing method. Uses a ReportingParseRunner (which only reports the first error) for simplicity.
      */
    def calculate(expression: String): Long = {
      val parsingResult = ReportingParseRunner(InputLine).run(expression)
      parsingResult.result match {
        case Some(i) => i
        case None =>
          throw new ParsingException(
            "Invalid calculation expression:\n" +
              ErrorUtils.printParseErrors(parsingResult))
      }
    }
  }
}
