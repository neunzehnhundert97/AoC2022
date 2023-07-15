import scala.io.Source

def solve(num: Int) =
  Source
    .fromFile("../ocaml/input01.data")
    .getLines
    .mkString("\n")
    .split("\n\n")
    .map(_.split("\n").map(_.toInt).sum)
    .sorted
    .takeRight(num)
    .sum

println(solve(1))
println(solve(3))
