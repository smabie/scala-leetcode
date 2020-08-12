/*
 * 94 Binary Tree Inorder Traversal
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

def inorderTraversal2(root: TreeNode): List[Int] = {
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
  xs
}
