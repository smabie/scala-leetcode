/*
 * 33 Search in Rotated Sorted Array
 */

def search(nums: Array[Int], target: Int): Int = {
  def go(lo: Int, hi: Int): Int = {
    val mid = (hi + lo + 1) / 2

    println(s"$lo $mid $hi")
    if (nums(hi) == target)
      hi
    else if (nums(lo) == target)
      lo
    else if (hi - lo <= 1)
      -1
    else if (nums(lo) <= nums(mid)) {
      if (target <= nums(mid) && target >= nums(lo))
        go(lo, mid)
      else
        go(mid, hi)
    } else {
      if (target >= nums(mid) && target <= nums(hi))
        go(mid, hi)
      else
        go(lo, mid)
    }
  }

  if (nums.isEmpty) -1
  else go(0, nums.length - 1)
}

search(Array(4, 5, 6, 7, 0, 1, 2), 5)