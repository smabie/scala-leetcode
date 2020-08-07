/*
 * 1448 Count Good Nodes in Binary Tree
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

def goodNodes(root: TreeNode): Int = {
  var num = 0
  def go(max: Int, node: TreeNode): Unit = {
    if (node == null)
      ()
    else if (node.value >= max) {
        num += 1
        go(node.value, node.left)
        go(node.value, node.right)
    } else {
        go(max, node.left)
        go(max, node.right)
    }
  }
  go(Int.MinValue, root)
  num
}

val x = T(3, T(1, T(3)), T(4, T(1), T(5)))

goodNodes(x)