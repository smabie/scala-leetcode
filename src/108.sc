/*
 * 108 Convert Sorted Array to Binary Search Tree
 */

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object T {
  def apply(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) = {
    new TreeNode(_value, _left, _right)
  }
}

def sortedArrayToBST(nums: Array[Int]): TreeNode = {
  def go(lo: Int, hi: Int): TreeNode = {
    val mid = (lo + hi) / 2
    if (hi < lo)
      null
    else
      new TreeNode(nums(mid), go(lo, mid - 1), go(mid + 1, hi))
  }
  go(0, nums.length - 1)
}



