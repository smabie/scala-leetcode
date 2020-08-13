/*
 * 515 Find Largest Value in Each Tree Row
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

def largestValues(root: TreeNode): List[Int] = {
  val ht = collection.mutable.SortedMap.empty[Int, Int]

  def go(node: TreeNode, depth: Int): Unit = {
    if (node == null)
      ()
    else {
      ht(depth) = ht.get(depth).fold(node.value)(math.max(_, node.value))
      go(node.left, depth + 1)
      go(node.right, depth + 1)
    }
  }
  go(root, 0)
  ht.values.toList
}

val x = T(1, T(3, T(5), T(3)), T(2, null, T(9)))

largestValues(x)