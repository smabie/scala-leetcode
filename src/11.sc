/*
 * 11 Container With Most water
 */

def maxArea(height: Array[Int]) = {
  def vol(s: Int, e: Int) = {
    math.min(height(s), height(e)) * (e - s)
  }

  val xs = height.zipWithIndex.scanLeft (0 -> 0) { case ((h, ix), (nh, nix)) =>
    if (h >= nh)
      h -> ix
    else
      nh -> nix
  }.tail

  val ys = height.zipWithIndex.scanRight (0 -> 0) { case ((nh, nix), (h, ix)) =>
    if (h >= nh)
      h -> ix
    else
      nh -> nix
  }.take(height.length)

  xs.zip(ys).map { case ((hx, ix), (hy, iy)) =>
    math.min(hx, hy) * (iy - ix)
  }
}


maxArea(Array(1, 2, 1))


/*
 min(i, j) * (ij - ix)
 */