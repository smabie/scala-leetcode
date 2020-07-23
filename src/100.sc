/*
 * 100 Same Tree
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


def isSameTree(p: TreeNode, q: TreeNode): Boolean = {
  def go(x: TreeNode, y: TreeNode): Boolean = {
    if (x == null && y == null)
      true
    else if (x == null || y == null)
      false
    else
      x.value == y.value && go(x.left, y.left) && go(x.right, y.right)
  }
  go(p, q)
}

val in = T(1, T(2), T(3))
val in2 = T(1, T(2))

isSameTree(in, in2)