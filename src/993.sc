/*
 * 993 Cousins Binary Tree
 *
 *
 */

 class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
   var value: Int = _value
   var left: TreeNode = _left
   var right: TreeNode = _right
 }

object T {
  def apply(v: Int, l: TreeNode = null, r: TreeNode = null) = new TreeNode(v, l, r)
}

def isCousins(root: TreeNode, x: Int, y: Int): Boolean = {
  def go(acc: Int, parent: TreeNode, node: TreeNode, x: Int): Option[(Int, TreeNode)] = {
    if (node == null)
      None
    else if (x == node.value)
      Some(acc -> parent)
    else
      go(acc + 1, node, node.left, x) orElse go(acc + 1, node, node.right, x)
  }

  val Some(xv -> xp) = go(0, null, root, x)
  val Some(yv -> yp) = go(0, null, root, y)

  xv == yv && xp != yp
}


val tr = T(1, T(2, T(4)), T(3))
val tr2 = T(1, T(2, r = T(4)), T(3, r = T(5)))
val tr3 = T(1, T(2, r = T(4)), T(3))

isCousins(tr2, 5, 4)