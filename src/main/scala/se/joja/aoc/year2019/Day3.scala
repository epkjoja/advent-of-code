package se.joja.aoc.year2019

import se.joja.joja.getInput

object Day3 extends App {
  val input = getInput("2019/day3.txt")

  val regex = """([LRUD])(\d+)""".r

  case class Point(x: Int = 0, y: Int = 0, steps: Set[Int] = Set(0)) {
    def isSame(other: Point): Boolean = x == other.x && y == other.y
    def distance: Int                 = Math.abs(x) + Math.abs(y)

    def left  = this.copy(x = x - 1, steps = Set(steps.max + 1))
    def right = this.copy(x = x + 1, steps = Set(steps.max + 1))
    def up    = this.copy(y = y + 1, steps = Set(steps.max + 1))
    def down  = this.copy(y = y - 1, steps = Set(steps.max + 1))
  }

  case class Matrix(points: Map[Int, List[Point]] = Map.empty) {
    def plot(track: List[String], start: Point = Point()): Matrix = {
      if (track.isEmpty) this
      else {
        val newPoints = track.head match {
          case regex("L", dx) => 1.to(dx.toInt).foldLeft(List(start))((acc, _) => acc :+ acc.last.left)
          case regex("R", dx) => 1.to(dx.toInt).foldLeft(List(start))((acc, _) => acc :+ acc.last.right)
          case regex("U", dy) => 1.to(dy.toInt).foldLeft(List(start))((acc, _) => acc :+ acc.last.up)
          case regex("D", dy) => 1.to(dy.toInt).foldLeft(List(start))((acc, _) => acc :+ acc.last.down)
        }

        println(s"New points: $newPoints")

        val newMatrix = newPoints.foldLeft(this) { (acc, p) =>
          acc.addPoint(p)
        }

        newMatrix.plot(track.tail, newPoints.last)
      }
    }

    def intersections(other: Matrix): List[(Point, Point)] = {
      points.toList.flatMap {
        case (row, pointsOnRow) =>
          other.points.getOrElse(row, List.empty).flatMap { otherPoint =>
            pointsOnRow.find(_.isSame(otherPoint)) match {
              case Some(value) => Some(value, otherPoint)
              case None        => None
            }
          }
      }
    }

    private def addPoint(p: Point): Matrix = {
      val newPointList = points.get(p.x) match {
        case None                                              => List(p)
        case Some(pointList) if !pointList.exists(_.isSame(p)) => pointList :+ p
        case Some(pointList) =>
          pointList.map {
            case point if point.isSame(p) => point.copy(steps = point.steps ++ p.steps)
            case point                    => point
          }
      }

      Matrix(points + (p.x -> newPointList))
    }
  }

  val tracks = input.map(line => line.split(",").toList)

  val matr1 = Matrix().plot(tracks.head)
  val matr2 = Matrix().plot(tracks.last)

  println(matr1)
  println(matr2)

  val intersections = matr1.intersections(matr2)
  val dists         = intersections.map { case (p1, p2) => p1.distance -> p1 }.sortBy(_._1)
  println(dists)
  println(s"Intersects: ${dists.drop(1).head}")

  // Part two

  val closest = intersections
    .map {
      case (p1, p2) =>
        p1.steps.min + p2.steps.min -> (p1, p2)
    }
    .sortBy(_._1)

  println(s"Closest: $closest")
}
