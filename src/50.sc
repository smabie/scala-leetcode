def myPow2(x: Double, n: Int) = {
  def go(x: Double, n: Int): Double = {
    if (n == 1)
      x
    else if (n % 2 == 0)
      go(x, n / 2) * go(x, n / 2)
    else
      x * go(x, n / 2) * go(x, n / 2)
  }

  if (x == 1d || n == 0)
    1d
  else if (n < 0)
    1 / go(x, math.abs(n))
  else
    go(x, n)
}

def myPow(x: Double, n: Long): Double = {
  @scala.annotation.tailrec
  def go(acc: Double, x: Double, n: Long): Double = {
    if (n < 0)
      go(acc, 1 / x, -n)
    else if (n == 0)
      acc
    else if (n == 1)
      acc * x
    else if (n % 2 == 0)
      go(acc, x * x, n / 2)
    else
      go(acc * x, x * x, n / 2)
  }
  go(1, x, n)
}


myPow(1.0, -2147483648)