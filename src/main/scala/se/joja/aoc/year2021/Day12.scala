package se.joja.aoc.year2021

import se.joja.aoc.getInput

object Day12 extends App {

  val input = getInput("2021/day12.txt")
  println(input)

  // Part 1
  case class Cave(name: String, adjacent: List[String]) {
    def addAdj(adj: String): Cave = this.copy(adjacent = adjacent :+ adj)
    def isBig: Boolean            = name.matches("[A-Z]+")
  }

  val CaveRegex = """([a-zA-Z]+)-([a-zA-Z]+)""".r
  val caves = input.foldLeft(Map.empty[String, Cave]) {
    case (acc, line) =>
      def update(in: Map[String, Cave], key: String, adj: String): Map[String, Cave] = {
        in.updatedWith(key) {
          case Some(value) => Some(value.addAdj(adj))
          case None        => Some(Cave(key, List(adj)))
        }
      }

      line match {
        case CaveRegex("start", b) => update(acc, "start", b)
        case CaveRegex(a, "start") => update(acc, "start", a)
        case CaveRegex("end", b)   => update(acc, b, "end")
        case CaveRegex(a, "end")   => update(acc, a, "end")
        case CaveRegex(a, b)       => update(update(acc, a, b), b, a)
      }
  }

  println(caves.mkString("\n"))

  type Path = List[String]

  def canVisit(cave: String, path: Path): Boolean =
    if (cave.matches("[a-z]+"))
      !path.contains(cave)
    else
      true

  def buildPaths(before: Path): List[Path] =
    before match {
      case "end" :: _ => List(before)
      case head :: _ =>
        val newPaths =
          caves.get(head).toList.flatMap(_.adjacent.filter(a => canVisit(a, before))).map(before.prepended)
        newPaths.flatMap(buildPaths)
    }

  val paths = buildPaths(List("start"))
  println(paths.map(_.reverse.mkString(",")).mkString("\n"))

  val result1 = paths.size
  println(s"Result part 1: $result1")

  // Part 2

  def caveIsSmall: String => Boolean = _.matches("[a-z]+")

  def canVisit2(cave: String, path: Path): Boolean =
    if (caveIsSmall(cave)) {
      val counts = path.filter(caveIsSmall).groupMapReduce(identity)(_ => 1)(_ + _)
      counts.getOrElse(cave, 0) match {
        case 0 => true
        case 1 => counts.forall(_._2 <= 1)
        case 2 => false
      }
    } else
      true

  def buildPaths2(before: Path): List[Path] =
    before match {
      case "end" :: _ => List(before)
      case head :: _ =>
        val newPaths =
          caves.get(head).toList.flatMap(_.adjacent.filter(a => canVisit2(a, before))).map(before.prepended)
        newPaths.flatMap(buildPaths2)
    }

  val paths2 = buildPaths2(List("start"))
  println(paths2.map(_.reverse.mkString(",")).sorted.mkString("\n"))
  val result2 = paths2.size

  println(s"Result part 2: $result2")

}
