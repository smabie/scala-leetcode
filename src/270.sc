/*
 * 270 Closest Binary Search Tree Value
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

def closestValue(root: TreeNode, target: Double): Int = {
  def close(x: Option[Int], y: Double) = {
    x.map { n =>
      math.abs(n - y)
    }
  }

  def go(node: TreeNode): Option[Int] = {
    if (node == null) {
      None
    } else {
      val l = go(node.left)
      val r = go(node.right)

      val x = close(l, target)
      val y = close(Some(node.value), target)
      val z = close(r, target)

      val mn = Some(List(x, y, z).flatten.min)

      if (mn == x)
        l
      else if (mn == y)
        Some(node.value)
      else
        r
    }
  }
  go(root).get
}


val x = T(4, T(2, T(1), T(3)), T(5))

closestValue(x, 3.714286)