/*
 * 1110 Delete Nodes And Return Forest
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

def delNodes(root: TreeNode, to_delete: Array[Int]): List[TreeNode] = {
  var trees = List.empty[TreeNode]

  def go(node: TreeNode, parent: TreeNode, isleft: Boolean): Unit = {
    if (node == null)
      ()
    else {
      go(node.left, node, true)
      go(node.right, node, false)

      if (to_delete.contains(node.value)) {
        if (parent != null) {
          if (isleft)
            parent.left = null
          else
            parent.right = null
        }
        if (node.left != null) trees = node.left :: trees
        if (node.right != null) trees = node.right :: trees
      }
    }
  }
  go(root, null, true)
  if (to_delete.contains(root.value))
    trees
  else
    root :: trees
}

val x = T(1, T(2, T(4), T(5)), T(3, T(6), T(7)))

delNodes(x, Array(3, 5))