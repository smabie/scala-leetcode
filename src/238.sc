/*
 * 238 Product of Array Except Self
 */

def productExceptSelf(nums: Array[Int]): Array[Int] = {
  val xs = nums.scanLeft(1)(_ * _).take(nums.length)
  val ys = nums.scanRight(1)(_ * _).tail

  println(xs.toList)
  println(ys.toList)

  xs.zip(ys).map{ case (x, y) => x * y }
}

productExceptSelf(Array(2, 2, 3, 4))


/*
    1   1  2   6   24
24 24 12   4   1


 */