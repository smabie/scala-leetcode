/*
 * 102 Binary Tree Level Order Traversal
 */

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
 }

object T {
  def apply(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null): TreeNode = {
    new TreeNode(_value, _left, _right)
  }
}

def levelOrder(root: TreeNode): List[List[Int]] = {
  var ht = collection.mutable.SortedMap.empty[Int, List[Int]]
  def go(node: TreeNode, level: Int): Unit = {
    if (node == null)
      ()
    else {
      go(node.right, level + 1)
      ht(level) = ht.get(level).fold(List(node.value))(node.value :: _)
      go(node.left, level + 1)
    }
  }
  go(root, 0)
  ht.values.toList
}

val x = T(3, T(9), T(20, T(15), T(7)))

levelOrder(x)