import scala.io.Source
@main def hello() = {

  val tuples_list = Source.stdin.getLines().map { line =>
    val parts = line.split("\\s+(?=\\d+$)").map(_.trim)
    if (parts.length == 2) (parts(0).toInt, parts(1).toInt)
    else throw new IllegalArgumentException("Expected 2 inputs for each line")
  }.toList

  val (a, b) = tuples_list.unzip

  val result_a = a.iterator.zip(b.iterator).map { t =>
    (t(1) - t(0)).abs
  }.reduce(_ + _)

  def toMap(x: Int): Map[Int, Int] = {
    Map(x -> 1)
  }

  def add(x: Map[Int, Int], y: Map[Int, Int]): Map[Int, Int] = {
    (x ++ y).map { (k, v) =>
      (k, x.getOrElse(k, 0) + y.getOrElse(k, 0))
    }
  }

  val b_counter = b.iterator.map(toMap).reduce(add)

  val result_b = a.map { x =>
    x * b_counter.getOrElse(x, 0)
  }.reduce(_ + _)


  println(s"$result_a")
  println(s"$result_b")

}
