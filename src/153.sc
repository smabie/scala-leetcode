/*
 * 153 Find Minimum in Rotated Sorted Array
 */

def findMin(nums: Array[Int]): Int = {
    def go(lo: Int, hi: Int): Int = {
    val mid = (lo + hi) / 2

    if (lo == mid)
      nums(hi)
    else if (nums(lo) <= nums(mid))
      go(mid, hi)
    else
      go(lo, mid)
  }
  if (nums.head < nums.last) nums.head
  else go(0, nums.length - 1)
}

findMin(Array(1, 2))

/*
lo  hi  mid
0   4   2
2   4   3
2   3   2
2   3


 */