/*
 * 199 Binary Tree Right Side View
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

def rightSideView(root: TreeNode): List[Int] = {
  val ht = collection.mutable.SortedMap.empty[Int, Int]
  def go(node: TreeNode, depth: Int): Unit = {
    if (node == null)
      ()
    else {
      if (!ht.contains(depth))
        ht(depth) = node.value
      go(node.right, depth + 1)
      go(node.left, depth + 1)
    }
  }
  go(root, 0)
  ht.values.toList
}

val x = T(1, T(2, null, T(5, T(6))), T(3, null, T(4)))

rightSideView(x)