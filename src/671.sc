/*
 * 571 Second Minimum Node in a Binary Tree
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

def findSecondMinimumValue(root: TreeNode): Int = {
  val set = collection.mutable.SortedSet.empty[Int]
  def go(node: TreeNode): Unit = {
    if (node == null)
      ()
    else {
      set += node.value
      go(node.left)
      go(node.right)
    }
  }
  go(root)
  set.toList.drop(1).headOption getOrElse -1
}

val x = T(2, T(2), T(5, T(5), T(7)))

findSecondMinimumValue(T(2, T(2), T(2)))