package se.joja.aoc.year2020

import se.joja.aoc.getInput

import scala.annotation.tailrec

object Day25 extends App {

  val input = getInput("2020/day25.txt").map(_.toLong)

  def transform(subject: Long, privateKey: Int): Long = {
    @tailrec
    def inner(loop: Int, acc: Long = 1): Long =
      if (loop <= 0) acc
      else
        inner(loop - 1, acc * subject % 20201227)

    inner(privateKey)
  }

  def findPrivateKey(subject: Long, publicKey: Long): Int = {
    @tailrec
    def trans(oldTerm: Long = 1, loop: Int = 0): Int = {
      if (oldTerm == publicKey) loop
      else
        trans(oldTerm * subject % 20201227, loop + 1)
    }

    trans()
  }

  val privKeys = input.map(pubKey => findPrivateKey(7, pubKey))
  println(privKeys)


  val encKeys = input.zip(privKeys.reverse).map {
    case (pubKey, privKey) =>
      transform(pubKey, privKey)
  }

  println(encKeys)
}
