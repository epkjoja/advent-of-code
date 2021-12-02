package se.joja.aoc.year2019

import se.joja.aoc.{getInput, invertMap}

object Day8 extends App {
  val input = getInput("2019/day8.txt")

  val (size, data) = (input.head, input.last)

  val wide :: tall :: Nil = size.split(",").toList.map(_.toInt)

  println(s"W: $wide, T: $tall, Data: $data")

  val allLayers = data.toList.grouped(wide * tall).toList.map(_.grouped(wide).toList.map(_.mkString))

  println(allLayers.map(_.mkString("\n")).mkString("\n\n"))

  val numZerosOnLayer = allLayers.map { layer =>
    layer.map(_.count(_ == '0')).sum -> layer
  }.toMap

  val fewestZeros = numZerosOnLayer.minBy(_._1)._2

  val (ones, twos) = (fewestZeros.map(_.count(_ == '1')).sum, fewestZeros.map(_.count(_ == '2')).sum)

  println(s"Result: ${ones * twos}")

  // Part two

  val allPixels = (for {
    x <- 0 until tall
    y <- 0 until wide
  } yield (x, y)).toList

  val finalPixels = allPixels.map { case (x, y) =>
    val pixelStack = allLayers.map(l => l(x).charAt(y))
    val finalColor = pixelStack.dropWhile(_ == '2').head
    (x, y, finalColor)
  }

  val finalStrings = finalPixels.groupBy(_._1).toList.sortBy(_._1).map(_._2.map(_._3).mkString)

  println(finalStrings.map(_.replace('0', ' ')).mkString("\n"))
}
