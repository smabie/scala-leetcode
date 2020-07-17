/*
 * 965 univalued binary tree
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

  def nTrue(node: TreeNode) = {
    if (node == null) true
  }
}

def isUnivalTree(root: TreeNode): Boolean = {
  def go(node: TreeNode): Boolean = {
    if (node == null)
      true
    else {
      val l = node.left == null || (node.left.value == node.value && go(node.left))
      val r = node.right == null || (node.right.value == node.value && go(node.right))
      l && r
    }
  }
  go(root)
}


val in = T(1, T(1, T(1), T(1)), T(1, null, T(1)))
val in2 = T(2, T(2, T(5), T(2)), T(2))

isUnivalTree(in)