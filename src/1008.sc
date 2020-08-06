/*
 * 1008 Construct Binary search Tree from Preorder Traversal
 */

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}


def bstFromPreorder(preorder: Array[Int]): TreeNode = {
  def go(ls: List[Int]): TreeNode = {
    if (ls.isEmpty)
      null
    else {
      val (xs, ys) = ls.tail.span(_ < ls.head)
      new TreeNode(ls.head, go(xs), go(ys))
    }
  }
  go(preorder.toList)
}

val x = bstFromPreorder(Array(8, 5, 1, 7, 10, 12))

x.left.left.value