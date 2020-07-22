/*
 * 606 Construct String from Binary Tree
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

def tree2str(t: TreeNode): String = {
  def go(node: TreeNode): String = {
    if (node == null)
      ""
    else if (node.left == null && node.right == null)
      s"${node.value}"
    else if (node.left == null && node.right != null)
      s"${node.value}()(${go(node.right)})"
    else if (node.left != null && node.right == null)
      s"${node.value}(${go(node.left)})"
    else {
      s"${node.value}(${go(node.left)})(${go(node.right)})"
    }
  }
  go(t)
}

val in = T(1, T(2, T(4)), T(3))
val in2 = T(1, T(2, null, T(4)), T(3))
tree2str(in2)