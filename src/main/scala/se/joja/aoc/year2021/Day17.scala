package se.joja.aoc.year2021

object Day17 extends App {

  // Part 1

  case class Target(xMin: Int, xMax: Int, yMin: Int, yMax: Int) {
    def hit(p: P): Boolean = alive(p) && p.x >= xMin && p.y <= yMax
    def alive(p: P): Boolean = p.x <= xMax && p.y >= yMin
  }

  case class P(x: Int, y: Int, vx: Int, vy: Int) {
    def step: P = P(x + vx, y + vy, vx - vx.sign, vy - 1)
    def trajectory(target: Target): (List[P], Boolean) = {
      val newP = this.step
      (target.hit(newP), target.alive(newP)) match {
        case (true, _) => List(this, newP) -> true
        case (false, true) =>
          val (list, hit) = newP.trajectory(target)
          (List(this) ++ list) -> hit
        case (false, false) => List(this) -> false
      }
    }
  }

  //val target = Target(20, 30, -10, -5)
  val target = Target(117, 164, -140, -89)
  val vxMax = target.xMax / 2

  val allInitials = for {
    vx <- 1.to(vxMax)
    vy <- 1.to(2000)
  } yield P(0, 0, vx, vy)

  val allHits = allInitials.flatMap { p =>
    val (list, hit) = p.trajectory(target)
    if (hit) Some(list) else None
  }.toList
  println(allHits.mkString("\n"))

  val maxY = allHits.map(_.maxBy(_.y).y).max
  println(s"Result part 1: $maxY")

  // Part 2

  val vyMin = target.yMin / 2
  println(s"vyMin: $vyMin")

  val allInitials2 = for {
    vx <- 1.to(target.xMax)
    vy <- target.yMin.to(1000)
  } yield P(0, 0, vx, vy)

  val allHits2 = allInitials2.flatMap { p =>
    val (list, hit) = p.trajectory(target)
    if (hit) Some(list) else None
  }.toList
  println(allHits2.mkString("\n"))

  val allHitVelocities = allHits2.map(p => p.head.vx -> p.head.vy)
  println(allHitVelocities)
  val result2 = allHitVelocities.size
  println(s"Result part 2: $result2")
}
