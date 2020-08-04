/*
 * 687 Longest Univalue Path
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

def longestUnivaluePath(root: TreeNode): Int = {
  var path = Int.MinValue
  def go(node: TreeNode): Int = {
    if (node == null)
      0
    else {
      val l = go(node.left)
      val r = go(node.right)


      val llen = if (node.left != null && node.left.value == node.value)
        1 + l else 0
      val rlen = if (node.right != null && node.right.value == node.value)
        1 + r else 0

      path = math.max(path, llen + rlen)
      math.max(llen, rlen)
    }
  }
  go(root)
  if (path == Int.MinValue) 0 else path
}

val x = T(5, T(4, T(1), T(1)), T(5, null, T(7)))
val y = T(1, T(4, T(4), T(4)), T(5, null, T(5)))

/*
    1
  4  5
4 4 5
*/
longestUnivaluePath(y)