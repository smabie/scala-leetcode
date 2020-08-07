/*
 * 814 Binary Tree Pruning
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

def pruneTree(root: TreeNode): TreeNode = {
  def go(node: TreeNode): TreeNode = {
    if (node == null)
      null
    else {
      val l = go(node.left)
      val r = go(node.right)

      if (node.value == 0 && l == null && r == null)
        null
      else
        new TreeNode(node.value, l, r)
    }
  }
  go(root)
}

val x = T(1, null, T(0, T(0), T(1)))

val y = pruneTree(x)

