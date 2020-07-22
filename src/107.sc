/*
 * 107 Binary Tree Level Order Traversal II
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


val in = T(3, T(9), T(20, T(15), T(7)))

def levelOrderBottom(root: TreeNode): List[List[Int]] = {
  val ht = collection.mutable.SortedMap.empty[Int, List[Int]](Ordering.Int.reverse)

  def go(depth: Int, node: TreeNode): Unit = {
    if (node == null)
      ()
    else {
      go(depth + 1, node.right)
      go(depth + 1, node.left)
      ht.get(depth).fold {
        ht(depth) = List(node.value)
      } { xs =>
        ht(depth) = node.value :: xs
      }
    }
  }
  go(0, root)
  ht.values.toList
}

levelOrderBottom(in)