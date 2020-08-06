/*
 * 1302 Deepest Leaves Sum
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

def deepestLeavesSum2(root: TreeNode): Int = {
  val ht = collection.mutable.SortedMap.empty[Int, Int](Ordering.Int.reverse)

  def go(node: TreeNode, depth: Int): Unit = {
    if (node == null)
      ()
    else {
      ht(depth) = ht.get(depth).fold {
         node.value
      } { n =>
        node.value + n
      }
      go(node.left, depth + 1)
      go(node.right, depth + 1)
    }
  }
  go(root, 0)
  ht.head._2
}

def deepestLeavesSum(root: TreeNode): Int = {
  var depth = 0
  var sum = 0

  def go(node: TreeNode, d: Int): Unit = {
    if (node == null)
      ()
    else {
      if (d == depth)
        sum += node.value
      else if (d > depth) {
        depth = d
        sum = node.value
      }
      go(node.left, d + 1)
      go(node.right, d + 1)
    }
  }
  go(root, 0)
  sum
}

val x = T(1, T(2, T(4, T(7)), T(5)), T(3, null, T(6, null, T(8))))

deepestLeavesSum2(x)
