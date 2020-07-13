/*
 * 283 Move Zeroes
 *
 * Given an array nums, write a function to move all 0's to the end of it while maintaining
 * the relative order of the non-zero elements.
 */
def moveZeroes(nums: Array[Int]): Unit = {
  var i = 0
  var ix = 0
  while (i < nums.length) {
    if (nums(i) != 0) {
      nums(ix) = nums(i)
      ix += 1
    }
    i += 1
  }
  while (ix < nums.length) {
    nums(ix) = 0
    ix += 1
  }
}

//moveZeroes(Array(2, 1))
moveZeroes(Array(0, 1, 0, 3, 12))