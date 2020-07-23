/*
 * 235 Lowest Common Ancestor of a Binary Search Tree
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

def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
  def go(node: TreeNode): Option[TreeNode] = {
    if (node == null)
      None
    else {
      val l = go(node.left)
      val r = go(node.right)
      val x = if (node.value == p.value || node.value == q.value)
          Some(node)
        else
          None
      (for {
        ll <- l
        xx <- x
      } yield node).orElse {
        for {
          rr <- r
          xx <- x
        } yield node
      }.orElse {
        for {
          ll <- l
          rr <- r
        } yield node
      }.orElse(l).orElse(r).orElse(x)
    }
  }
  go(root).get
}


val in = T(6, T(2, T(0), T(4, T(3), T(5))), T(8, T(7), T(9)))

lowestCommonAncestor(in, in.left, in.left.right).value