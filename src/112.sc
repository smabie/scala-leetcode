/*
 * 112 Path Sum
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

def hasPathSum(root: TreeNode, sum: Int): Boolean = {
  var hasSum = false
  def go(node: TreeNode, sum: Int): Unit = {
    if (node == null)
      ()
    else if (node.left == null && node.right == null)
      hasSum = hasSum || sum - node.value == 0
    else {
      go(node.left, sum - node.value)
      go(node.right, sum - node.value)
    }
  }
  go(root, sum)
  hasSum
}

val x = T(1, T(2))

hasPathSum(x, 1)