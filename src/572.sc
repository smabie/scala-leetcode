/*
 * 572 Subtree of Another Tree
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


def isSubtree(s: TreeNode, t: TreeNode) = {
  def go(node: TreeNode): String = {
    if (node == null)
      "n"
    else
      s"#${node.value} ${go(node.left)} ${go(node.right)}"
  }
  go(s).contains(go(t))
}

val a = T(3, T(4, T(1), T(2)), T(5))

isSubtree(a, T(4, T(1), T(2)))