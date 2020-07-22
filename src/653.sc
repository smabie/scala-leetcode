/*
 * 653 Two Sum IV - Input is a BST
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


def findTarget(root: TreeNode, k: Int): Boolean = {
  val ht = collection.mutable.Set.empty[Int]
  var found = false

  def go(node: TreeNode): Unit = {
    if (node == null || found)
      ()
    else if (ht.contains(node.value))
        found = true
    else {
        ht.add(k - node.value)
        go(node.left)
        go(node.right)
    }
  }
  go(root)
  found
}

val in = T(5, T(3, T(2), T(4)), T(6, null, T(7)))

findTarget(in, 7)