/*
 * 563 Binary Tree Tilt
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

def findTilt(root: TreeNode): Int = {
  case class Sum(tsum: Int, sum: Int)

  def go(node: TreeNode): Sum = {
    if (node == null)
      Sum(0, 0)
    else {
      val l = go(node.left)
      val r = go(node.right)
      Sum(l.tsum + r.tsum + math.abs(l.sum - r.sum), l.sum + r.sum + node.value)
    }
  }
  go(root).tsum
}

val x = T(1, T(2), T(3))

findTilt(x)