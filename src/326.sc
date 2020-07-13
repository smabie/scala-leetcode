/*
 * 326 Power of Three
 */

def isPowerOfThree(n: Int) = {
  val p = 0.00000000001
  val x = math.log(n.toDouble) / math.log(3)
  x < math.round(x) + p && x > math.round(x) - p
}

