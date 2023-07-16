import scala.io.Source
enum Shape {
  case Rock
  case Paper
  case Scisor

  def value = this match
    case Rock   => 1
    case Paper  => 2
    case Scisor => 3

  def value_against(other: Shape) = (this, other) match
    case (Rock, Scisor) | (Scisor, Paper) | (Paper, Rock) => 6
    case (a, b) if a == b                                 => 3
    case _                                                => 0

  def find_partner(outcome: String) = outcome match {
    // Loose
    case "X" => Shape.values((this.ordinal + 2) % 3)
    // Draw
    case "Y" => this
    // Win
    case "Z" => Shape.values((this.ordinal + 1) % 3)
  }

}

object Shape {

  def parse(input: String) = input match {
    case "A" | "X" => Rock
    case "B" | "Y" => Paper
    case "C" | "Z" => Scisor
  }

}

def round1() =
  println(
    Source
      .fromFile("../ocaml/input02.data")
      .getLines
      .map(_.split(' ').map(Shape.parse) match {
        case Array(a, b) => (a, b)
        case _           => throw Exception("Oh no!")
      })
      .map { (a, b) => b.value + b.value_against(a) }
      .sum
  )

def round2() =
  println(
    Source
      .fromFile("../ocaml/input02.data")
      .getLines
      .map(_.split(' ') match {
        case Array(a, b) =>
          val enemy = Shape.parse(a)
          (enemy, enemy.find_partner(b))
        case _ => throw Exception("Oh no!")
      })
      .map { (a, b) => b.value + b.value_against(a) }
      .sum
  )

round1()
round2()
