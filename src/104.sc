/*
 * 104 Maximum Depth of Binary Tree
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

def maxDepth2(root: TreeNode): Int = {
  def go(node: TreeNode): Int = {
    if (node.left == null && node.right == null)
      1
    else if (node.left != null && node.right == null)
      1 + go(node.left)
    else if (node.right != null && node.left == null)
      1 + go(node.right)
    else
      math.max(1 + go(node.left), 1 + go(node.right))
  }
  if (root == null) 0 else go(root)
}

def maxDepth(root: TreeNode): Int = {
  def go(node: TreeNode): Int = {
    if (node.left == null && node.right == null)
      1
    else {
      math.max(if (node.left == null) 0 else 1 + go(node.left),
        if (node.right == null) 0 else 1 + go(node.right)
      )
    }
  }
  if (root == null) 0 else go(root)
}



val in = T(3, T(9), T(20, T(15), T(7)))

maxDepth(in)