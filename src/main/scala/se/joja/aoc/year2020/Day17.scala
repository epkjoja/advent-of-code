package se.joja.aoc.year2020

import se.joja.aoc.getInput

object Day17 extends App {

  val input = getInput("2020/day17.txt")

  val side = input.size

  case class Point(x: Int, y: Int, z: Int, w: Int = 0) {
    def neighbours: List[Point] = {
      (for {
        dx <- -1 to 1
        dy <- -1 to 1
        dz <- -1 to 1
        dw <- -1 to 1
      } yield Point(dx + x, dy + y, dz + z, dw + w))
        .filterNot(_.isSame(this))
        .toList
    }

    def isSame(p: Point): Boolean = p.x == x && p.y == y && p.z == z && p.w == w
  }

  val points = input.zipWithIndex.flatMap {
    case (x, xi) =>
      x.zipWithIndex.filter(_._1 == '#').map {
        case ('#', yi) => Point(xi, yi, 0)
      }
  }

  def maxMin(list: List[Point], f: Point => Int): (Int, Int) = {
    val vals = list.map(f)
    (vals.min, vals.max)
  }

  def nextGen(points: List[Point]): List[Point] = {
    val (xMin, xMax) = maxMin(points, _.x)
    val (yMin, yMax) = maxMin(points, _.y)
    val (zMin, zMax) = maxMin(points, _.z)
    val (wMin, wMax) = maxMin(points, _.w)

    (for {
      dx <- (xMin - 1) to (xMax + 1)
      dy <- (yMin - 1) to (yMax + 1)
      dz <- (zMin - 1) to (zMax + 1)
      dw <- (wMin - 1) to (wMax + 1)
    } yield {
      val p           = Point(dx, dy, dz, dw)
      val isActive    = points.exists(_.isSame(p))
      val activeCount = p.neighbours.count(x => points.exists(_.isSame(x)))

      (isActive, activeCount) match {
        case (true, 2)  => Some(p)
        case (true, 3)  => Some(p)
        case (true, _)  => None
        case (false, 3) => Some(p)
        case (false, _) => None
      }
    }).flatten.toList
  }

  def printPoints(points: List[Point]): Unit = {
    val (xMin, xMax) = maxMin(points, _.x)
    val (yMin, yMax) = maxMin(points, _.y)

    points.groupBy(_.z).toList.sortBy(_._1).foreach {
      case (zi, xyPoints) =>
        println(s"z = $zi")
        val xMap = xyPoints.groupBy(_.x)
        xMin.to(xMax).foreach { x =>
          xMap.get(x) match {
            case None => println(".".repeat(yMax - yMin + 1))
            case Some(ys) =>
              yMin.to(yMax).foreach { y =>
                if (ys.exists(_.y == y)) print("#") else print(".")
              }
              println("")
          }
        }
    }
  }

  val finalPoints = 1.to(6).foldLeft(points) {
    case (acc, cycle) =>
      val newPoints = nextGen(acc)
      println(s"After cycle $cycle")
      printPoints(newPoints)
      newPoints
  }

  println(s"Result: ${finalPoints.size}")
}
