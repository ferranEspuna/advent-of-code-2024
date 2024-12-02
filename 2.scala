import scala.io.Source
@main def hello() = {

  val tuples = Source.stdin.getLines().map { line =>
    line.split(" ").map(_.trim).map { x =>
      x.toInt
    }.toList
  }.toList

  def isGoodIncrement(x: Int): Boolean = {
    (1 <= x) && (x <= 3)
  }

  def isSafe(input: List[Int], isGoodStep: (Int, Int) => Boolean , canIgnore: Boolean = false): Boolean = {
    if (input.length <= 1) true
    else if (input.length == 2) canIgnore || isGoodStep(input(0), input(1))
    else if (! isGoodStep(input(0), input(1))) { // > 2 elements, first 2 not safe
      if (! canIgnore) false
      else if (! isGoodStep(input(0), input(2))) isSafe(input.drop(1), isGoodStep, false) // first and third also not safe --> remove first
      else isSafe(input.drop(2), isGoodStep, false) // first and third safe --> can check from third on, removing the first
    }
    else{ // > 2 elements, first and second safe
      if (! isGoodStep(input(1), input(2))) { // 2nd and 3rd not safe
        if (! canIgnore) false
        else if (input.length == 3) true // just ignore last (3rd) element
        else if (! (isGoodStep(input(0), input(2)) && isGoodStep(input(2), input(3)))) { // removing 2nd does not work --> remove third
          isGoodStep(input(1), input(3)) && isSafe(input.drop(3), isGoodStep, false) // 1st -- 2nd -- 4th -- rest
        }
        else isSafe(input.drop(2), isGoodStep, false) // if it will work somehow, just removing the 2nd will work, as 1st -- 3rd -- 4th -- rest
      }
      else isSafe(input.drop(1), isGoodStep, canIgnore) // 2nd and 3rd also safe --> first and second are there, free to proceed
    }
  }

  def isSafeAtAll(input: List[Int], canIgnore: Boolean): Boolean = {
    isSafe(input, (x, y) => isGoodIncrement(y - x), canIgnore) || isSafe(input, (x, y) => isGoodIncrement(x - y), canIgnore)
  }


  def asInt(x: Boolean): Int = if (x) 1 else 0

  val result_a = tuples.map{x => isSafeAtAll(x, false)}.map(asInt).reduce(_ + _)
  val result_b = tuples.map{x => isSafeAtAll(x, true)}.map(asInt).reduce(_ + _)

  println(s"$result_a")
  println(s"$result_b")

}
