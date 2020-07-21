/*
 * Trim a Binary Search Tree
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

def trimBST(root: TreeNode, L: Int, R: Int): TreeNode = {
  def go(node: TreeNode): TreeNode = {
    if (node == null)
      null
    else {
      if (node.value >= L && node.value <= R)
        new TreeNode(node.value, go(node.left), go(node.right))
      else if (node.value <= L)
        go(node.right)
      else
        go(node.left)

    }
  }
  go(root)
}








