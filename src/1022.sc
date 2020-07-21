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
def sumRootToLeaf2(root: TreeNode): Int = {
  var sum = 0

  def go(s: String, node: TreeNode): Unit = {
    if (node == null)
      ()
    else if (node.left == null && node.right == null) {
      val str = s ++ node.value.toString
      val n = Integer.parseInt(str, 2)
      sum += n
    } else {
      go(s ++ node.value.toString, node.left)
      go(s ++ node.value.toString, node.right)
    }
  }
  go("", root)
  sum
}

def sumRootToLeaf(root: TreeNode): Int = {
  var sum = 0
  def go(s: List[Int], node: TreeNode): Unit = {
    if (node == null) ()
    else if (node.left == null && node.right == null) {
      sum += (node.value :: s).foldLeft(0 -> 1) { case ((acc, n), digit) =>
        (acc + n * digit, n << 1)
      }._1
    } else {
      val v = node.value :: s
      go(v, node.left)
      go(v, node.right)
    }
  }
  go(List.empty[Int], root)
  sum
}




val in = T(1, T(0, T(0), T(1)), T(1, T(0), T(1)))

sumRootToLeaf(in)