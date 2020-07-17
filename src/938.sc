/*
 * 938 Range Sum of BST
 */

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object T {
  def apply(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null): Unit = {
    new TreeNode(_value, _left, _right)
  }
}

def rangeSumBST(root: TreeNode, L: Int, R: Int): Int = {
  def go(root: TreeNode): Int = {
    if (root == null)
      0
    else {
      (if (root.value <= R && root.value >= L)
        root.value
      else 0) + go(root.right) + go(root.left)
    }
  }
  go(root)
}

