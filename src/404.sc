/*
 * 404 Sum of Left Leaves
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


def sumOfLeftLeaves(root: TreeNode): Int = {
  var sum = 0

  def go(node: TreeNode): Unit = {
    if (node == null)
      ()
    else if (node.left != null) {
      if (node.left.left == null && node.left.right == null)
        sum += node.left.value
      else
        go(node.left)
      go(node.right)
    } else
      go(node.right)
  }
  go(root)
  sum
}

val in = T(3, T(9), T(20, T(15), T(7)))

sumOfLeftLeaves(in)