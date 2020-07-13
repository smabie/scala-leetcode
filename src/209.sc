/*
 * 209 Minimum Size Subarray Sum
 *
 * Given an array of n positive integers and a positive integer s, find the minimal
 * length of a contiguous subarray of which the sum ≥ s. If there isn't one, return 0 instead.
 *
 * No solution yet
 */
def minSubArrayLenN2(s: Int, nums: Array[Int]): Int = {
  (1 to nums.length).flatMap { n =>
    if (nums.sliding(n).map(_.sum).exists(_ >= s))
      Some(n)
    else
      None
  }.minOption getOrElse 0
}


def minSubArrayLen(s: Int, nums: Array[Int]): Int = {
  var sum = 0
  var start = 0
  var i = 0
  var mn = Int.MaxValue

  while (i < nums.length) {
    if (sum + nums(i) >= s) {
      mn = math.min(i - start + 1, mn)
      sum -= nums(start)
      start += 1
    } else {
      sum += nums(i)
      i += 1
    }
  }
  if (mn == Int.MaxValue) 0 else mn
}


minSubArrayLen(10, Array(2, 3, 1, 2, 4, 3))

