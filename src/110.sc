/*
 * 110 Balanced Binary Tree
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

def isBalanced(root: TreeNode) = {
  case class Data(bal: Boolean, height: Int)

  def go(node: TreeNode): Data = {
    if (node == null) {
      Data(true, 0)
    } else {
      val l = go(node.left)
      val r = go(node.right)

      val height = math.max(1 + l.height, 1 + r.height)

      Data(l.bal && r.bal && math.abs(l.height - r.height) < 2, height)
    }
  }
  go(root).bal
}

val x = T(3, T(9), T(20, T(15), T(7)))
val y = T(1, T(2, T(3, T(4), T(4)), T(3)), T(2))
isBalanced(y)