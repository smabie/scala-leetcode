/*
 * Merge Intervals
 */

def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
  intervals.sortBy(_.head).foldLeft(List.empty[Array[Int]]) {
    case (Nil, x) => x :: Nil
    case (ys@Array(x, y) :: xs, p@Array(i, j)) =>
      println(ys.map(_.mkString(",")))
      if (y < i)
        p :: ys
      else
        Array(math.min(x, i), math.max(y, j)) :: xs
  }.reverseIterator.toArray
}


merge(Array(Array(1, 4), Array(4, 5)))
//merge(Array(Array(1, 2), Array(2, 6), Array(8, 10), Array(15, 18)))