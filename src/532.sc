/*
 * 532 k-diff pairs in an array
 */

def findPairs(nums: Array[Int], k: Int) = {
/*
  |i - j| = k
  i - j = k
  i - j = -k

  j = i - k
  j = i + k
 */

  case class Pair(i: Int, j: Int) {
    override def hashCode(): Int = {
      i.hashCode() + j.hashCode()
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case Pair(x, y) => (i == x && j == y) || (i == y && j == x)
        case _ => false
      }
    }
  }

  val found = collection.mutable.Set.empty[Pair]
  val pot = collection.mutable.Set.empty[(Int, Int)]

  nums.foreach { i =>
    if (pot.contains(i -> (i - k)) && math.abs (i - (i - k)) == k)
      found += Pair(i, i - k)
    if (pot.contains(i -> (i + k)) && math.abs (i - (i + k)) == k)
      found += Pair(i, i + k)

    pot += (i - k) -> i
    pot += (i + k) -> i
  }
  found.size
}

findPairs(Array(6, 3, 5, 7, 2, 3, 3, 8, 2, 4), 2)
