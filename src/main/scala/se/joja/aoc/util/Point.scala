package se.joja.aoc.util

case class Point(x: Int, y: Int) {
  def dist(that: Point): Int =
    (x - that.x).abs + (y - that.y).abs

  def isAdjecent(p: Point): Boolean =
    (p.x - x).abs <= 1 && (p.y - y).abs <= 1

  def line(to: Point): List[Point] = {
    val (xDir, yDir) = (to.x - this.x, to.y - this.y)

    val l = (xDir, yDir) match {
      case (x, _) if x > 0 => this.x.to(to.x).map(i => this.copy(x = i))
      case (x, _) if x < 0 => to.x.to(this.x).map(i => this.copy(x = i))
      case (_, y) if y > 0 => this.y.to(to.y).map(i => this.copy(y = i))
      case (_, y) if y < 0 => to.y.to(this.y).map(i => this.copy(y = i))
    }
    l.toList
  }
}


