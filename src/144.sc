/*
 * 144 Binary Tree Preorder Traversal
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

def preorderTraversal(root: TreeNode): List[Int] = {
  var xs = List.empty[Int]
  def go(node: TreeNode): Unit = {
    if (node == null)
      ()
    else {
      go(node.right)
      go(node.left)
      xs = node.value :: xs
    }
  }
  go(root)
  xs
}


val x = T(1, null, T(2, T(3)))

preorderTraversal(x)