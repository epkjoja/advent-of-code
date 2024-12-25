package se.joja.aoc.util

case class Point(x: Int, y: Int) {
  def diff(that: Point): (Int, Int) =
    (x - that.x, y - that.y)

  def dist(that: Point): Int = {
    val (dx, dy) = diff(that)
    dx.abs + dy.abs
  }

  def isAdjecent(p: Point): Boolean =
    p != this && (p.x - x).abs <= 1 && (p.y - y).abs <= 1

  def neighbours4: List[Point] =
    List(Point(x, y + 1), Point(x + 1, y), Point(x, y - 1), Point(x - 1, y))

  def line(to: Point): List[Point] = {
    val (xDir, yDir) = (to.x - this.x, to.y - this.y)

    ((xDir, yDir) match {
      case (x, _) if x > 0 => this.x.to(to.x).map(i => this.copy(x = i))
      case (x, _) if x < 0 => this.x.to(to.x, -1).map(i => this.copy(x = i))
      case (_, y) if y > 0 => this.y.to(to.y).map(i => this.copy(y = i))
      case (_, y) if y < 0 => this.y.to(to.y, -1).map(i => this.copy(y = i))
    }).toList
  }

  def up: Point = this.copy(x = this.x - 1)
  def down: Point = this.copy(x = this.x + 1)
  def left: Point = this.copy(y = this.y - 1)
  def right: Point = this.copy(y = this.y + 1)

  def move(dir: String): Point = {
    dir match {
      case "U" | "N" | "^" => up
      case "D" | "S" | "v" => down
      case "L" | "W" | "<" => left
      case "R" | "E" | ">" => right
      case "NE" => up.right
      case "NW" => up.left
      case "SE" => down.right
      case "SW" => down.left
      case _ => this
    }
  }

  def move(dir: Char): Point = move(dir.toString)

  def line(dir: String, steps: Int = 1): List[Point] = {
    if (steps == 0) List(this) else {
      this :: move(dir).line(dir, steps - 1)
    }
  }

  def swap: Point = Point(y, x)
}

/**
 * En area med X neråt/vertikalt och Y horisontellt/på raden
 */
case class Area[A](a: Vector[Vector[A]], topLeft: Point = Point(0, 0)) {
  def count(p: A => Boolean): Long =
    a.map(_.count(p)).sum

  def isInside(p: Point): Boolean =
    p.x >= topLeft.x && p.x < topLeft.x + a.length && p.y >= topLeft.y && p.y < topLeft.y + a.head.length

  def isValid(p: Point, pred: A => Boolean): Boolean =
    isInside(p) && pred(a(p.x)(p.y))

  def filter(pred: A => Boolean): List[(Point, A)] =
    (for {
      x <- a.indices
      y <- a.head.indices
      v = a(x)(y)
      p = Point(x + topLeft.x, y + topLeft.y) if pred(v)
    } yield (p, v)).toList

  def fillInside(isBorder: A => Boolean, fill: A): Area[A] = {
    val newArea = a.map { line =>
      val (newLine, _) = line.foldLeft((List.empty[A], false)) { case ((acc, isInside), a) =>
        val newA = if (isInside) fill else a
        val newInside = if (isBorder(a)) !isInside else isInside
        (acc :+ newA, newInside)
      }
      newLine.toVector
    }
    this.copy(a = newArea)
  }

  def updated(point: Point, data: A): Area[A] =
    this.copy(a = a.updated(point.x, a(point.x).updated(point.y, data)))

  override def toString: String =
    a.map(_.mkString("")).mkString("\n")

  def apply(point: Point): A =
    a(point.x)(point.y)
}

object Area {

  def apply[A](l: List[(Point, A)], default: A): Area[A] = {
    val (minX, minY) = (l.minBy(_._1.x)._1.x, l.minBy(_._1.y)._1.y)
    val (maxX, maxY) = (l.maxBy(_._1.x)._1.x, l.maxBy(_._1.y)._1.y)

//    println(s"Area boundaries x: $minX-$maxX, y: $minY-$maxY")

    val v = l.groupBy(_._1.x).map { case (x, pointsY) =>
      x -> pointsY.foldLeft(Vector.fill(maxY - minY + 1)(default)) { case (acc, (p, f)) =>
        acc.updated(p.y - minY, f)
      }
    }.foldLeft(Vector.fill(maxX - minX + 1)(Vector.fill(maxY - minY + 1)(default))) { case (acc, (x, v)) =>
      acc.updated(x - minX, v)
    }
    Area(v, Point(minX, minY))
  }

  /**
   * Create a filled area from two corner points
   */
  def apply[A](p1: Point, p2: Point, fill: A, default: A): Area[A] = {
    val minX :: maxX :: Nil = Seq(p1.x, p2.x).sorted
    val minY :: maxY :: Nil = Seq(p1.y, p2.y).sorted

    val points = for {
      x <- minX.to(maxX)
      y <- minY.to(maxY)
    } yield Point(x, y)

    Area(points.toList.map(_ -> fill), default)
  }
}
