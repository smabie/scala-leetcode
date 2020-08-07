/*
 * 1325 Delete Leaves With a Given Value
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

def removeLeafNodes(root: TreeNode, target: Int): TreeNode = {
  def go(node: TreeNode): TreeNode = {
    if (node == null)
      null
    else {
      val l = go(node.left)
      val r = go(node.right)

      if (l == null && r == null && node.value == target)
        null
      else
        new TreeNode(node.value, l, r)
    }
  }
  go(root)
}

val x = T(1, T(2, T(2)), T(3, T(2), T(4)))

val y = removeLeafNodes(x, 2)