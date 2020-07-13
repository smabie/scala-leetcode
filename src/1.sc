/*
 * 1 Two Sum
 *
 * Given an array of integers, return indices of the two numbers such that
 * they add up to a specific target.
 */

def twoSum(nums: Array[Int], target: Int): Array[Int] = {
  val ht = scala.collection.mutable.HashMap.empty[Int, Int]

  nums.zipWithIndex.flatMap { case (n, ix) =>
    ht.get(n).fold {
      ht(target - n) = ix
      Array.empty[Int]
    } { iy =>
      Array(ix, iy)
    }
  }
}

twoSum(Array(2, 7, 11, 5), 9)