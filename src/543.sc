/*
 * 543 Diameter of Binary Tree
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

def diameterOfBinaryTree2(root: TreeNode): Int = {
  case class V(maxLen: Int, maxDiam: Int)

  def go(root: TreeNode): V = {
    if (root.left == null && root.right == null)
      V(0, 0)
    else if (root.left == null) {
      val V(ml, md) = go(root.right)
      V(ml + 1, math.max(md, ml + 1))
    } else if (root.right == null) {
      val V(ml, md) = go(root.left)
      V(ml + 1, math.max(md, ml + 1))
    } else {
      val V(ml, md) = go(root.left)
      val V(ml2, md2) = go(root.right)

      V(1 + math.max(ml, ml2), math.max(2 + ml + ml2, math.max(md, md2)))
    }
  }
  if (root == null) 0 else go(root).maxDiam
}

def diameterOfBinaryTree(node: TreeNode): Int = {
  var max = 0

  def go(root: TreeNode): Int = {
    if (root == null)
      0
    else {
      val l = go(root.left)
      val r = go(root.right)

      max = math.max(1 + l + r, max)
      math.max(l, r) + 1
    }
  }
  go(node) - 1
}

val x = T(1, T(1, T(1), T(1, T(1))))

diameterOfBinaryTree(x)