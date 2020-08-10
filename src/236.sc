/*
 * 236 Lowest Common ANcestor of a Binary Tree
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
  case class Found(p: Boolean, q: Boolean, t: TreeNode)

  def go(node: TreeNode): Found = {
    if (node == null)
      Found(p = false, q = false, null)
    else {
      val f = Found(p = node.value == p.value, q = node.value == q.value, null)
      val l = go(node.left)
      val r = go(node.right)

      if (l.t != null) l
      else if (r.t != null) r
      else {
        val res = Found(p = f.p || l.p || r.p, q = f.q || l.q || r.q, null)

        if (res.q && res.p)
          res.copy(t = node)
        else res
      }
    }
  }
  go(root).t
}

val x = T(3, T(5, T(6), T(2, T(7), T(4))), T(1, T(0), T(8)))

lowestCommonAncestor(x, T(6), T(4)).value