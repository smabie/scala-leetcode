/*
 * 617 Merge Two Binary Trees
 */

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object T {
  def apply(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null)= {
    new TreeNode(_value, _left, _right)
  }
}

def mergeTrees(t1: TreeNode, t2: TreeNode): TreeNode = {
  def go(x: TreeNode, y: TreeNode): TreeNode = {
    x -> y match {
      case null -> null => null
      case null -> y => y
      case x -> null => x
      case x -> y => new TreeNode(x.value + y.value, go(x.left, y.left), go(x.right, y.right))
    }
  }
  go(t1, t2)
}

val t1 = T(1, T(3, T(4)), T(2))
val t2 = T(2, T(1, null, T(4)), T(3, null, T(7)))

val x = mergeTrees(t1, t2)
