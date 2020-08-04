/*
 * 101 Symmetric Tree>
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


def isSymmetric(root: TreeNode)  = {
  def go(x: TreeNode, y: TreeNode): Boolean = {
    if (x == null && y == null)
      true
    else if (x != null && y != null)
      x.value == y.value && go(x.left, y.right) && go(x.right, y.left)
    else
      false
  }
  if (root == null)
    true
  else
    go(root.left, root.right)
}

val x = T(1, T(2, T(3), T(5)), T(2, T(4), T(3)))

isSymmetric(x)