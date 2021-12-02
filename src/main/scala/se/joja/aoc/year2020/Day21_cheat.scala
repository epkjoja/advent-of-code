package se.joja.aoc.year2020

import se.joja.aoc.getInput

object Day21_cheat {

  case class Food(ingredients: Set[String], allergens: Set[String])

  def findAllergenIngredients(foods: Seq[Food]): Map[String, Set[String]] = {
    val j = (for {
      Food(ingredients, allergens) <- foods
      allergen <- allergens
    } yield allergen -> ingredients)
    j.groupMapReduce(_._1)(_._2)(_ & _)
  }

  def findGoodIngredients(foods: Seq[Food]): Set[String] = {
    val allergenIngredients = findAllergenIngredients(foods)
    println(allergenIngredients.mkString("\n"))
    val allIngredients = foods.view.flatMap(_.ingredients).toSet
    val allAllergenIngredients = allergenIngredients.values.flatten.toSet
    allIngredients -- allAllergenIngredients
  }

  def countGoodIngredients(foods: Seq[Food]): Int = {
    val goodIngredients = findGoodIngredients(foods)
    foods
      .map(_.ingredients.count(goodIngredients))
      .sum
  }

  def solveAllergenIngredients(foods: Seq[Food]): Map[String, String] = {

    def helper(allergenIngredients: Map[String, Set[String]]): Iterator[Map[String, String]] = {
      if (allergenIngredients.isEmpty)
        Iterator(Map.empty)
      else {
        val (allergen, ingredients) = allergenIngredients.head
        val newAllergenIngredients = allergenIngredients - allergen
        for {
          ingredient <- ingredients.iterator
          newAllergenIngredients2 = newAllergenIngredients.view.mapValues(_ - ingredient).toMap
          rest <- helper(newAllergenIngredients2)
        } yield rest + (allergen -> ingredient)
      }
    }

    val allergenIngredients = findAllergenIngredients(foods)
    helper(allergenIngredients).next()
  }

  def canonicalBadIngredients(foods: Seq[Food]): String = {
    val allergenIngredients = solveAllergenIngredients(foods)

    allergenIngredients
      .toSeq
      .sortBy(_._1)
      .map(_._2)
      .mkString(",")
  }


  private val foodRegex = """(.*) \(contains (.*)\)""".r

  def parseFood(s: String): Food = s match {
    case foodRegex(ingredients, allergens) =>
      Food(
        ingredients.split(" ").toSet,
        allergens.split(", ").toSet
      )
  }

  def parseFoods(input: List[String]): Seq[Food] = input.map(parseFood).toSeq

  lazy val input = getInput("2020/day21.txt")

  def main(args: Array[String]): Unit = {
    println(countGoodIngredients(parseFoods(input)))
    println(canonicalBadIngredients(parseFoods(input)))
  }
}
