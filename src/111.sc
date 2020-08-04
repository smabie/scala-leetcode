/*
 * Minimum Depth of Binary Tree
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

def minDepth(root: TreeNode): Int = {
  var minDepth = Int.MaxValue
  def go(node: TreeNode, depth: Int): Unit = {
    if (node == null)
      ()
    else if (node.left == null && node.right == null) {
      minDepth = math.min(minDepth, 1 + depth)
    } else {
      go(node.left, depth + 1)
      go(node.right, depth + 1)
    }
  }
  go(root, 0)
  if (minDepth == Int.MaxValue) 0 else minDepth
}

val x = T(3, T(9), T(20, T(15), T(7)))
val y = T(3, T(1), T(5))
val z = T(3, T(9), T(20, T(15), T(7)))

minDepth(z)