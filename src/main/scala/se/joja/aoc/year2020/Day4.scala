package se.joja.aoc.year2020

import scala.io.Source

import se.joja.aoc.splitOnEmptyLine

object Day4 extends App {

  val input = Source.fromResource("2020/day4.txt").getLines().toList

//  byr (Birth Year)
//  iyr (Issue Year)
//  eyr (Expiration Year)
//  hgt (Height)
//  hcl (Hair Color)
//  ecl (Eye Color)
//  pid (Passport ID)
//  cid (Country ID)


//  byr (Birth Year) - four digits; at least 1920 and at most 2002.
//  iyr (Issue Year) - four digits; at least 2010 and at most 2020.
//  eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
//  hgt (Height) - a number followed by either cm or in:
//    If cm, the number must be at least 150 and at most 193.
//    If in, the number must be at least 59 and at most 76.
//  hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
//  ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
//  pid (Passport ID) - a nine-digit number, including leading zeroes.
//  cid (Country ID) - ignored, missing or not.


  val mandMarks = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  case class Data(data: Map[String, String] = Map.empty) {
    def isEmpty: Boolean = data.isEmpty
    def isValid: Boolean = mandMarks.forall(data.contains)
    def isValidTwo: Boolean = {
      val checks = data.map {
        case ("byr", v) => v.toIntOption.exists(b => b >= 1920 && b <= 2002)
        case ("iyr", v) => v.toIntOption.exists(b => b >= 2010 && b <= 2020)
        case ("eyr", v) => v.toIntOption.exists(b => b >= 2020 && b <= 2030)
        case ("hgt", v) =>
          val (num, unit) = v.span(_.isDigit)
          (num.toInt, unit) match {
            case (n, "cm") => n >= 150 && n <= 193
            case (n, "in") => n >= 59 && n <= 76
            case _ => false
          }
        case ("hcl", v) => v.matches("#[0-9a-f]{6}")
        case ("ecl", v) => Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(v)
        case ("pid", v) => v.matches("[0-9]{9}")
        case ("cid", _) => true
        case _ => false
      }
      !checks.toSeq.contains(false)
    }
  }

  val dataList = splitOnEmptyLine(input).map { strings =>
    val marks = strings.mkString(" ").split(" ").map(x => x.take(3) -> x.drop(4)).toMap
    Data(marks)
  }

  //dataList.foreach(println)

  val colons = dataList.map(_.data.size).sum
  println(s"Num of marks: $colons")

  val (valid, invalid) = dataList.partition(_.isValid)
  println(s"Valid: ${valid.size}, Invalid: ${invalid.size}")

  val (valid2, invalid2) = dataList.partition(d => d.isValid && d.isValidTwo)
  println(s"Valid2: ${valid2.size}, Invalid2: ${invalid2.size}")
}
