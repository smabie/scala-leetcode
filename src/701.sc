/*
 * 701 Insert Into a Binary Search Tree
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


def insertIntoBST(root: TreeNode, `val`: Int): TreeNode = {
  def go(node: TreeNode): Unit = {
    if (node == null)
      ()
    else {
      if (node.value > `val`) {
        if (node.left == null)
          node.left = new TreeNode(`val`)
        else
          go(node.left)
      } else {
        if (node.right == null)
          node.right = new TreeNode(`val`)
        else
          go(node.right)
      }
    }
  }
  if (root == null) new TreeNode(`val`)
  else {
    go(root)
    root
  }
}

val x = T(4, T(2, T(1), T(3)), T(7))

val y = insertIntoBST(x, 5)

y.right.left.value