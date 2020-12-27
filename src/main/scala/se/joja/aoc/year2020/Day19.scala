package se.joja.aoc.year2020

import se.joja.joja.{getInput, splitOnEmptyLine}

object Day19 extends App {

  val input = getInput("2020/day19.txt")

  val (rules :: msgs :: Nil) = splitOnEmptyLine(input)

  sealed trait Rule {
    def parse(s: String): List[String]
  }

  case class AndRule(num: String, a: Rule, b: Rule) extends Rule {
    override def parse(s: String): List[String] = a.parse(s).flatMap(b.parse)
  }

  case class And3Rule(num: String, a: Rule, b: Rule, c: Rule) extends Rule {
    override def parse(s: String): List[String] =
      for {
        aRes <- a.parse(s)
        bRes <- b.parse(aRes)
        cRes <- c.parse(bRes)
      } yield cRes
  }

  case class OrRule(num: String, a: Rule, b: Rule) extends Rule {
    override def parse(s: String): List[String] = a.parse(s) ++ b.parse(s)
  }

  case class CharRule(num: String, a: Char) extends Rule {
    override def parse(s: String): List[String] =
      if (s.isEmpty) List.empty else if (s.head == a) List(s.tail) else List.empty
  }

  case object StopRule extends Rule {
    override def parse(s: String): List[String] = List.empty
  }

  val And1Re = """(\d+): (\d+)""".r
  val And2Re = """(\d+): (\d+) (\d+)""".r
  val And3Re = """(\d+): (\d+) (\d+) (\d+)""".r
  val Or11Re = """(\d+): (\d+) \| (\d+)""".r
  val Or12Re = """(\d+): (\d+) \| (\d+) (\d+)""".r
  val Or22Re = """(\d+): (\d+) (\d+) \| (\d+) (\d+)""".r
  val Or23Re = """(\d+): (\d+) (\d+) \| (\d+) (\d+) (\d+)""".r
  val CharRe = """(\d+): "(\w)"""".r

  val rulesMap = rules.map(r => r.takeWhile(_.isDigit).toInt -> r).toMap

  def getRule(num: Int): Rule = {
    rulesMap.getOrElse(num, throw new Exception(s"Rule $num doesn't exist")) match {
      case And1Re(_, a)         => getRule(a.toInt)
      case And2Re(num, a, b)    => AndRule(num, getRule(a.toInt), getRule(b.toInt))
      case And3Re(num, a, b, c) => And3Rule(num, getRule(a.toInt), getRule(b.toInt), getRule(c.toInt))
      case Or22Re(num, a, b, c, d) =>
        OrRule(num,
               AndRule(s"${num}a", getRule(a.toInt), getRule(b.toInt)),
               AndRule(s"${num}b", getRule(c.toInt), getRule(d.toInt)))
      case Or11Re(num, a, b) => OrRule(num, getRule(a.toInt), getRule(b.toInt))
      case CharRe(num, c)    => CharRule(num, c.head)
    }
  }

  val parser = getRule(0)

  val leftover = msgs.map(parser.parse)

  //println(leftover.mkString("\n"))

  val res1 = leftover.count(_.exists(_.isEmpty))
  println(s"Result: $res1")

  // Part two

  val rules2Map = rulesMap + (8 -> "8: 42 | 42 8") + (11 -> "11: 42 31 | 42 11 31")

  //println(rules2Map.toList.sorted.mkString("\n"))

  def getRule2(num: Int, depth: Int = 0): Rule = {
    rules2Map.getOrElse(num, throw new Exception(s"Rule $num doesn't exist")) match {
      case And1Re(_, a)                       => getRule2(a.toInt)
      case And2Re(num, a, b)                  => AndRule(num, getRule2(a.toInt), getRule2(b.toInt))
      case And3Re(num, a, b, c)               => And3Rule(num, getRule2(a.toInt), getRule2(b.toInt), getRule2(c.toInt))
      case Or11Re(num, a, b)                  => OrRule(num, getRule2(a.toInt), getRule2(b.toInt))
      case Or12Re(num, _, _, _) if depth > 10 => StopRule
      case Or12Re(num, a, b, c) =>
        OrRule(num, getRule2(a.toInt), AndRule(s"${num}b", getRule2(b.toInt), getRule2(c.toInt, depth + 1)))
      case Or22Re(num, a, b, c, d) =>
        OrRule(num,
               AndRule(s"${num}a", getRule2(a.toInt), getRule2(b.toInt)),
               AndRule(s"${num}b", getRule2(c.toInt), getRule2(d.toInt)))
      case Or23Re(num, _, _, _, _, _) if depth > 10 => StopRule
      case Or23Re(num, a, b, c, d, e) =>
        OrRule(num,
               AndRule(s"${num}a", getRule2(a.toInt), getRule2(b.toInt)),
               And3Rule(s"${num}b", getRule2(c.toInt), getRule2(d.toInt, depth + 1), getRule2(e.toInt)))
      case CharRe(num, c) => CharRule(num, c.head)
    }
  }

  val parser2 = getRule2(0)

  val leftover2 = msgs.map(parser2.parse)

  println(leftover2.mkString("\n"))

  val res2 = leftover2.count(_.exists(_.isEmpty))
  println(s"Result2: $res2")
}
