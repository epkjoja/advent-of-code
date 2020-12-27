package se.joja.aoc.year2020

import se.joja.joja.{getInput, splitOnEmptyLine}

object Day20 extends App {

  val input = getInput("2020/day20.txt")

  val rawTiles = splitOnEmptyLine(input)

  type Matrix = List[String]

  case class Tile(num: Long, mat: Matrix, orientation: Int = 0) {
    def orient(orientation: Int): Tile = {
      (orientation / 4, orientation % 4) match {
        case (0, rot) => rotate(rot)
        case (1, rot) => flip.rotate(rot)
      }
    }

    private def flip: Tile = Tile(num, mat.transpose.map(_.mkString), (orientation + 4) % 8)

    private def rotate(r: Int): Tile = {
      val (currFlip, currRot) = (orientation / 4, orientation % 4)
      println(s"rotate ($num) - newRot: $r, currFlip: $currFlip, currRot: $currRot")
      r match {
        case 0 => this
        case 1 => Tile(num, mat.transpose.map(_.mkString.reverse), currFlip * 4 + (currRot + 1) % 4)
        case 2 => Tile(num, mat.reverse.map(_.reverse), currFlip * 4 + (currRot + 2) % 4)
        case 3 => Tile(num, mat.transpose.map(_.mkString).reverse, currFlip * 4 + (currRot + 3) % 4)
      }
    }

    def rotateUntil(p: Tile => Boolean): Tile = {
      0.until(8)
        .foldLeft(Option.empty[Tile]) {
          case (acc, r) =>
            val test = orient(r)
            (acc, p(test)) match {
              case (t @ Some(_), _) => t
              case (None, true)     => Some(test)
              case _                => None
            }
        }
        .get
    }

    def edges: Set[Int] = {
      val e = List(mat.head, mat.map(_.last).mkString, mat.last, mat.map(_.head).mkString)
      e.flatMap(s => List(Integer.parseInt(s, 2), Integer.parseInt(s.reverse, 2))).toSet
    }

    def edgeUp: Int    = binToInt(mat.head)
    def edgeDown: Int  = binToInt(mat.last)
    def edgeLeft: Int  = binToInt(mat.map(_.head).mkString)
    def edgeRight: Int = binToInt(mat.map(_.last).mkString)

    def removeBorder(): Tile =
      copy(mat = mat.slice(1, mat.size - 1).map(_.slice(1, mat.size - 1)))

    def countPattern(pat: List[String]): Int = {

      def checkString(str: String, patStr: String): Boolean = {
        patStr.zip(str).forall {
          case ('1', s) => s == '1'
          case ('0', _) => true
        }
      }

      val allTests = for {
        y <- 0.until(mat.size - pat.size)
        x <- 0.until(mat.head.length - pat.head.length)
      } yield {
        pat.indices.forall { patY =>
          checkString(mat(y + patY).drop(x), pat(patY))
        }
      }
      allTests.count(_ == true)
    }

    override def toString: String = {
      s"Tile: $num ($orientation) L: $edgeLeft, U: $edgeUp, D: $edgeDown, R: $edgeRight\n" +
        mat.mkString("\n")
    }
  }

  def binToInt(s: String): Int = if (s.length > 10) -1 else Integer.parseInt(s, 2)

  val Regex = """Tile (\d+):""".r

  val allTiles = rawTiles.map {
    case (Regex(tileNum) :: tileRows) =>
      val mat = tileRows.map { row =>
        row.replace('.', '0').replace('#', '1')
      }

      Tile(tileNum.toInt, mat)
  }

  val side = Math.sqrt(allTiles.size).toInt
  if (side * side != allTiles.size) throw new Exception(s"Invalid num of tiles: ${allTiles.size}")

  def takeFromList[A](l: List[A], first: Int, step: Int): List[A] =
    first.until(l.size, step).foldLeft(List.empty[A])((acc, i) => acc :+ l(i))

  def checkTiles(tiles: List[Tile]): Boolean = {
    def checkRow(tileRow: List[Tile]): Boolean =
      tileRow.sliding(2).forall { case first :: second :: Nil => first.edgeRight == second.edgeLeft }

    def checkCol(tileCol: List[Tile]): Boolean =
      tileCol.sliding(2).forall { case first :: second :: Nil => first.edgeDown == second.edgeUp }

    tiles.grouped(side).forall(checkRow) && 0.until(side).map(c => takeFromList(tiles, c, side)).forall(checkCol)
  }

  val allEdgesByEdge = allTiles.foldLeft(Map.empty[Int, List[Long]]) {
    case (acc, t) =>
      t.edges.foldLeft(acc) {
        case (innerAcc, e) =>
          innerAcc.get(e) match {
            case None       => innerAcc + (e -> List(t.num))
            case Some(list) => innerAcc + (e -> (list :+ t.num))
          }
      }
  }

  val allEdgesByTile = allTiles.map(t => t.num -> t.edges).toMap

  println(allEdgesByTile.mapValues(_.toList.sorted).mkString("\n"))

  def checkPositions(tiles: List[Tile]): Boolean = {
    def checkRowCol(tilePair: List[Tile]): Boolean =
      tilePair.sliding(2).forall {
        case first :: second :: Nil =>
          allEdgesByTile(first.num).intersect(allEdgesByTile(second.num)).nonEmpty
      }

    tiles.grouped(side).forall(checkRowCol) && 0.until(side).map(c => takeFromList(tiles, c, side)).forall(checkRowCol)
  }

  val possibleNeighbours = allTiles.map { tile =>
    tile.num -> allEdgesByTile(tile.num).flatMap(e => allEdgesByEdge(e).filterNot(_ == tile.num))
  }.toMap

  println(s"Possible neighbours:\n${possibleNeighbours.mkString("\n")}")

  val corners = possibleNeighbours.filter(_._2.size < 3)

  println(s"Result: $corners, ${corners.keySet.product}")

  // Part two

  def findSide(path: List[Long], end: List[Long]): List[List[Long]] = {
    //println(s"findSide - path: $path, end: $end")
    if (path.size == side) {
      if (end.contains(path.last)) List(path) else List.empty
    } else {
      possibleNeighbours(path.last).toList.flatMap(t => findSide(path :+ t, end))
    }
  }

  val outerSides1 = findSide(List(corners.head._1), corners.tail.keys.toList)

  val upperLeft  = corners.head._1
  val upperRight = outerSides1.head.last
  val lowerLeft  = outerSides1.last.last
  val lowerRight = corners.keys.toList.diff(List(upperLeft, upperRight, lowerLeft)).head
  //println(s"All sides:\n${outerSides1.mkString("\n")}")

  println(s"Upper left/right: $upperLeft, $upperRight")
  println(s"Lower left/right: $lowerLeft, $lowerRight")

  val leftSide  = outerSides1.last
  val rightSide = findSide(List(upperRight), List(lowerRight)).head

  println(s"Left side: $leftSide")
  println(s"Right side: $rightSide")

  val orderedTiles = 0
    .until(side)
    .flatMap { i =>
      val row = findSide(List(leftSide(i)), List(rightSide(i))).head
      row.map(t => allTiles.find(_.num == t).get)
    }
    .toList

  println(s"Ordered tiles: ${orderedTiles.map(_.num)}")

  val commonRight = allEdgesByTile(orderedTiles.head.num).intersect(allEdgesByTile(orderedTiles(1).num))
  val commonDown  = allEdgesByTile(orderedTiles.head.num).intersect(allEdgesByTile(orderedTiles(side).num))
  val cornerCorrect = orderedTiles.head.rotateUntil { t =>
    commonRight.contains(t.edgeRight) && commonDown.contains(t.edgeDown)
  }

  println(s"CornerCorrect: \n$cornerCorrect")

  val orderedTiles2 = List(cornerCorrect) ++ orderedTiles.tail

  val allCorrect = (for {
    y <- 0 until side
    x <- 0 until side
  } yield (y, x)).foldLeft(List.empty[Tile]) {
    case (acc, (y, x)) =>
      if (x == 0 && y == 0) {
        acc :+ cornerCorrect
      } else if (x == 0) {
        val tile       = orderedTiles2(y * side + x)
        val lookingFor = acc((y - 1) * side).edgeDown
        val rotTile = tile.rotateUntil { t =>
          val res = t.edgeUp == lookingFor
          println(s"RotateUntil x0 (${t.num}), y/x: $y/$x, lookingFor: $lookingFor edgeUp: ${t.edgeUp}")
          res
        }
        println(s"Rotated tile:\n$rotTile")
        acc :+ rotTile
      } else {
        val tile       = orderedTiles2(y * side + x)
        val lookingFor = acc(y * side + x - 1).edgeRight
        val rotTile = tile.rotateUntil { t =>
          val res = t.edgeLeft == lookingFor
          println(s"RotateUntil (${t.num}), y/x: $y/$x, lookingFor: $lookingFor, edgeLeft: ${t.edgeLeft}")
          res
        }
        println(s"Rotated tile:\n$rotTile")
        acc :+ rotTile
      }
  }

  println(s"All correct: ${allCorrect.mkString("\n\n")}")

  val withoutBorders = allCorrect.map(_.removeBorder())

  println(s"Without borders: ${withoutBorders.mkString("\n\n")}")

  val bigMatrix = withoutBorders
    .sliding(side, side)
    .flatMap { rowTiles =>
      rowTiles.foldLeft(List.empty[String]) {
        case (acc, t) =>
          acc.zipAll(t.mat, "", "").map { p =>
            p._1 + p._2
          }
      }
    }
    .toList

  val bigTile = Tile(0, bigMatrix)
  println(s"Big tile:\n$bigTile")

  val monster =
    """                  # 
      |#    ##    ##    ###
      | #  #  #  #  #  #   """.stripMargin.replace(' ', '0').replace('#', '1').split('\n').toList

  println(monster.mkString("\n"))

  val monsterCounts = 0.until(8).map(i => bigTile.orient(i).countPattern(monster))

  println(s"Num monsters: $monsterCounts")

  val totalHashes = bigTile.mat.map(_.count(_ == '1')).sum
  val monsterHashes = monster.map(_.count(_ == '1')).sum

  val res2 = totalHashes - (monsterHashes * monsterCounts.max)
  println(s"Result2: $res2")
}
