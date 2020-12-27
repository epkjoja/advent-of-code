package se.joja.aoc.year2020

import se.joja.aoc.year2020.Day21_cheat.Food
import se.joja.joja.getInput

object Day21 extends App {

  val input = getInput("2020/day21.txt")

  val Regex = """([\w ]+) \(contains ([\w ,]+)\)""".r

  type Ingredient = String
  type Allergen   = String

  case class Rule(ingredients: Set[Ingredient], allergens: Set[Allergen])

  val allRules = input.map {
    case Regex(ingrs, allers) =>
      val i = ingrs.split(' ').toList
      val a = allers.split(", ").toList
      Rule(i.toSet, a.toSet)
  }

  // Allergen -> List(Set(Ingredients))
  val ingrsByAllers2 = allRules.foldLeft(Map.empty[Allergen, Set[Ingredient]]) {
    case (acc, r) =>
      r.allergens.toList.flatMap { a =>
        acc.get(a) match {
          case None       => acc + (a -> r.ingredients)
          case Some(list) => acc + (a -> (list & r.ingredients))
        }
      }.toMap
  }

  def findAllergenIngredients(foods: List[Rule]): Map[String, Set[String]] = {
    val j = (for {
      Rule(ingredients, allergens) <- foods
      allergen <- allergens
    } yield allergen -> ingredients)
    j.groupMapReduce(_._1)(_._2)(_ & _)
  }

  val ingrsByAllers = findAllergenIngredients(allRules)

  println("Intersects:")
  println(ingrsByAllers.mkString("\n"))

  val ingrWithAller    = ingrsByAllers.flatMap(_._2).toSet
  val allIngr          = allRules.flatMap(_.ingredients.toList)
  val ingrWithoutAller = allIngr.toSet.diff(ingrWithAller)

  val res = allIngr.count(ingrWithoutAller.contains)

  println(s"All count: ${allRules.map(_.ingredients.size).sum}")
  println(s"All count2: ${allIngr.size}")
  println(s"Result: ${res}")
}
