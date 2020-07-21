/*
 * 872 Leaf-Similar Trees
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

def leafSimilar(root1: TreeNode, root2: TreeNode): Boolean = {
  val x = collection.mutable.ListBuffer.empty[Int]
  val y = collection.mutable.ListBuffer.empty[Int]

  def go(lb: collection.mutable.ListBuffer[Int], node: TreeNode): Unit = {
    if (node == null)
      ()
    else if (node.left == null && node.right == null)
      lb.addOne(node.value)
    else {
      go(lb, node.left)
      go(lb, node.right)
    }
  }
  go(x, root1)
  go(y, root2)
  x == y
}

val x = T(1, T(), T(2))
val y = T(2, T(1), T(2))

leafSimilar(x, y)