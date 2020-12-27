package se.joja.aoc.year2020

import se.joja.joja.{getInput, splitOnEmptyLine}

import scala.annotation.tailrec

object Day22 extends App {

  val input = getInput("2020/day22.txt")

  type Deck = List[Int]

  val decks = splitOnEmptyLine(input).map(_.tail.map(_.toInt))

  @tailrec
  def playRound(decks: List[Deck], round: Int = 1): List[Deck] = {
    val (p1Deck :: p2Deck :: Nil) = decks

    println(s"-- Round $round --")
    println(s"Player 1 deck: $p1Deck")
    println(s"Player 2 deck: $p2Deck")

    (p1Deck, p2Deck) match {
      case (Nil, _) => decks
      case (_, Nil) => decks
      case (p1 :: p1tail, p2 :: p2tail) =>
        println(s"P1 plays: $p1, P2 plays $p2")

        if (p1 > p2) {
          println("P1 wins the round!")
          playRound(List(p1tail :+ p1 :+ p2, p2tail), round + 1)
        } else if (p2 > p1) {
          println("P2 wins the round!")
          playRound(List(p1tail, p2tail :+ p2 :+ p1), round + 1)
        } else
          throw new Exception(s"Can't handle same cards!")
    }
  }

  //val winner = playRound(decks).find(_.nonEmpty).get

  //val res = winner.reverse.zipWithIndex.map { case (card, i) => card * (i + 1)}.sum
  //println(s"Result: $res")

  // Part two

  def playRecRound(decks: List[Deck],
                   round: Int = 1,
                   game: Int = 1,
                   oldDecks: Set[(Deck, Deck)] = Set.empty): (Int, Deck) = {
    val (p1Deck :: p2Deck :: Nil) = decks

    println(s"\n-- Round $round (Game $game) --")
    println(s"Player 1 deck: $p1Deck")
    println(s"Player 2 deck: $p2Deck")

    (p1Deck, p2Deck) match {
      case _ if oldDecks.contains((p1Deck, p2Deck)) =>
        println(s"Decks has already been played. The winner of game $game is player 1!")
        (1, p1Deck)

      case (Nil, _) =>
        println(s"The winner of game $game is player 2!")
        (2, p2Deck)

      case (_, Nil) =>
        println(s"The winner of game $game is player 1!")
        (1, p1Deck)

      case (p1 :: p1tail, p2 :: p2tail) if p1tail.size >= p1 && p2tail.size >= p2 =>
        // Recursive Combat
        println(s"P1 plays: $p1, P2 plays: $p2")
        println("Playing a sub-game to determine the winner...")
        val res = playRecRound(List(p1tail.take(p1), p2tail.take(p2)), game = game + 1)

        println(s"...anyway, back to game $game.")
        if (res._1 == 1)
          playRecRound(List(p1tail :+ p1 :+ p2, p2tail), round + 1, game, oldDecks ++ Set((p1Deck, p2Deck)))
        else
          playRecRound(List(p1tail, p2tail :+ p2 :+ p1), round + 1, game, oldDecks ++ Set((p1Deck, p2Deck)))

      case (p1 :: p1tail, p2 :: p2tail) =>
        println(s"P1 plays: $p1, P2 plays: $p2")

        if (p1 > p2) {
          println(s"Player 1 wins round $round of game $game!")
          playRecRound(List(p1tail :+ p1 :+ p2, p2tail), round + 1, game, oldDecks ++ Set((p1Deck, p2Deck)))
        } else {
          println(s"Player 2 wins round $round of game $game!")
          playRecRound(List(p1tail, p2tail :+ p2 :+ p1), round + 1, game, oldDecks ++ Set((p1Deck, p2Deck)))
        }
    }
  }

  val winner2 = playRecRound(decks)

  val res2 = winner2._2.reverse.zipWithIndex.map { case (card, i) => card * (i + 1) }.sum
  println(s"Result2: Winner is: ${winner2._1}, Score: $res2, Deck: ${winner2._2}")
}
