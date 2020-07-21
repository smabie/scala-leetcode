/*
 * 226 Invert Binary Tree
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

def invertTree(root: TreeNode): TreeNode = {
  def go(node: TreeNode): Unit = {
    if (node == null) ()
    else {
      val tmp = node.left
      node.left = node.right
      node.right = tmp
      go(node.left)
      go(node.right)
    }
  }
  go(root)
  root
}