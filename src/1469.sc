

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

def getLonelyNodes(root: TreeNode): List[Int] = {
  def go(root: TreeNode): List[Int] = {
    if (root == null || (root.left == null && root.right == null))
      Nil
    else if (root.right != null && root.left != null)
      go(root.left) ++ go(root.right)
    else if (root.right != null)
      root.right.value :: go(root.right)
    else
      root.left.value :: go(root.left)
  }
  go(root)
}


val in = T(1, T(2, null, T(4)), T(3))
val in2 = T(7, T(1, T(6)), T(4, T(5), T(3, null, T(2))))

getLonelyNodes(in2)