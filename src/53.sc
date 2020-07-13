 /*
  * 53 Maximum Subarray
  */

 def maxSubArray(nums: Array[Int]) = {
   nums.foldLeft(0 -> List.empty[Int]) { case ((mx, xs), n) =>
     if (xs.isEmpty) n -> List(n)
     else {
       val ys = n :: xs.map(_ + n)
       math.max(ys.max, mx) -> ys
     }
   }._1
 }

 def maxSubArray2(nums: Array[Int]) = {
   nums.tail.foldLeft(nums(0) -> nums(0)) { case ((csum, msum), n) =>
     val x = math.max(n, n+csum)
     x -> math.max(msum, x)
   }._2
 }

 def maxSubArray3(nums: Array[Int]) = {
   var csum = nums(0)
   var msum = nums(0)
   val len = nums.length
   var i = 1
   while (i < len) {
     val n = nums(i)
     csum = math.max(n, n+csum)
     msum = math.max(csum, msum)
     i += 1
   }
   msum
 }

 maxSubArray3(Array(-2, 1, -3, 4, -1, 2, 1, -5, 4))