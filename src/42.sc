/*
 * 42 Trapping Rain Water
 * Given n non-negative integers representing an elevation map where the width of
 * each bar is 1, compute how much water it is able to trap after raining.
 */

/*
 * scanleft and scanright using max. find the min of both and subtract the number at
 * the current index
 */
def trap(height: Array[Int]): Int = {
  val l = height.scanLeft(0)(math.max)
  val r = height.scanRight(0)(math.max)

  height.zipWithIndex.foldLeft(0) { case (acc, (v, ix)) =>
    acc+math.max(math.min(l(ix+1), r(ix+1))-v, 0)
  }
}
trap(Array(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2))


