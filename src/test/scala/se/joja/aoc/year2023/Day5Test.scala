package se.joja.aoc.year2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import se.joja.aoc.year2023.Day5.Mapping

import scala.language.postfixOps

class Day5Test extends AnyFlatSpec with should.Matchers {

  it should "work with non overlapping mappings" in {
    Mapping(5, 10, 1).combine(Mapping(15, 20, 2)) shouldEqual(
      List(Mapping(5,10,1), Mapping(15,20,2))
    )
  }

  it should "work with touching mappings with same diff" in {
    Mapping(5, 10, 1).combine(Mapping(11, 20, 1)) shouldEqual (
      List(Mapping(5, 20, 1))
      )
  }

  it should "work with touching mappings with different diff" in {
    Mapping(5, 10, 1).combine(Mapping(11, 20, 2)) shouldEqual (
      List(Mapping(5,10,1), Mapping(11,20,2))
      )
  }

  it should "work with partly overlapping mappings" in {
    Mapping(5, 10, 1).combine(Mapping(9, 20, 2)) shouldEqual (
      List(Mapping(5, 8, 1), Mapping(9, 10, 3), Mapping(11, 20, 2))
      )
    Mapping(9, 20, 2).combine(Mapping(5, 10, 1)) shouldEqual (
      List(Mapping(5, 8, 1), Mapping(9, 10, 3), Mapping(11, 20, 2))
      )
  }

  it should "work with fully overlapping mappings" in {
    Mapping(1, 10, 1).combine(Mapping(5, 9, 2)) shouldEqual (
      List(Mapping(1, 4, 1), Mapping(5, 9, 3), Mapping(10, 10, 1))
      )
    Mapping(1, 10, 1).combine(Mapping(5, 10, 2)) shouldEqual (
      List(Mapping(1, 4, 1), Mapping(5, 10, 3))
      )
    Mapping(1, 10, 1).combine(Mapping(1, 5, 2)) shouldEqual (
      List(Mapping(1, 5, 3), Mapping(6, 10, 1))
      )
  }

  it should "work with same mappings" in {
    Mapping(1, 10, 1).combine(Mapping(1, 10, 2)) shouldEqual (
      List(Mapping(1, 10, 3))
      )
  }
}
