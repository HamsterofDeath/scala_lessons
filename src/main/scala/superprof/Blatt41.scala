package superprof

object Blatt41 {
  def union2(a: List[Int], b: List[Int]) = {
    var remainingA = a
    var remainingB = b
    var result     = List.empty[Int]
    while (remainingA.nonEmpty || remainingB.nonEmpty) {
      val nextA = remainingA.headOption
      val nextB = remainingB.headOption
      (nextA, nextB) match {
        case (Some(intA), Some(intB)) if intA < intB =>
          result = intA :: result
          remainingA = remainingA.tail
        case (Some(intA), Some(intB)) if intA > intB =>
          result = intB :: result
          remainingB = remainingB.tail
        case (Some(_), Some(intB)) =>
          result = intB :: result
          remainingA = remainingA.tail
          remainingB = remainingB.tail
        case (Some(intA), None) =>
          result = intA :: result
          remainingA = remainingA.tail
        case (None, Some(intB)) =>
          result = intB :: result
          remainingB = remainingB.tail
        case _ => throw new RuntimeException("can't happen")
      }
    }
    result.reverse
  }

  def union(a: List[Int], b: List[Int]) = {
    (a ++ b).distinct.sorted
  }
  def intersect(a: List[Int], b: List[Int]) = {
    a.filter(b.contains)
  }

  def maxOf(l: List[Int]) = {
    l.reduce(_ max _)
  }
  def maxOf2(l: List[Int]) = {
    l.fold(0)(_ max _)
  }
  def maxOf3(l: List[Int]) = {
    def biggerOfTwo(a: Int, b: Int) = if (a > b) a else b

    l.fold(0)(biggerOfTwo)
    l.fold(0)(biggerOfTwo(_, _))
    l.fold(0)((a, b) => biggerOfTwo(a, b))
  }

  def streiche(y: Int, l: List[Int]) = {
    l.indexOf(y) match {
      case -1 => l
      case n => l.take(n) ++ l.drop(n + 1)
    }
  }

  def maxSort(l: List[Int]): List[Int] = {
    if (l.isEmpty) l else maxOf(l) :: maxSort(streiche(maxOf(l), l))
  }

  def main(args: Array[String]): Unit = {
    println(union(List(1, 5, 17), List(3, 5, 18)))
    println(maxSort(List(5, 6, 4, 7, 3, 8, 1)))
  }
}

