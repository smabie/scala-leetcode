/*
 * 783 Minimum Distance Between BST Nodes
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

def minDiffInBST(root: TreeNode): Int = {
  var xs = List.empty[Int]

  def go(node: TreeNode): Unit = {
    if (node == null)
      ()
    else {
      go(node.right)
      xs = node.value :: xs
      go(node.left)
    }
  }
  go(root)
  xs.foldLeft(Int.MaxValue -> Int.MaxValue) { case ((p, min), n) =>
    n -> math.min(math.abs(p - n), min)
  }._2
}