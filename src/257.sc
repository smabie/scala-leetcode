/*
 * 257 Binary Tree Paths
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


def binaryTreePaths(root: TreeNode): List[String] = {
  var xs = List.empty[String]

  def go(s: String, node: TreeNode): Unit = {
    if (node == null)
      ()
    else if (node.left == null && node.right == null) {
      xs = s"$s${node.value}" :: xs
    } else {
      val str = s"$s${node.value}->"
      go(str, node.right)
      go(str, node.left)
    }
  }
  go("", root)
  xs
}

val in = T(1, T(2, null, T(5)), T(3))

binaryTreePaths(in)