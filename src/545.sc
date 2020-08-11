/*
 * 545 Bounary of Binary Tree
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

def boundaryOfBinaryTree(root: TreeNode) = {
  if (root == null)
    return List.empty[Int]
  val left = {
    var xs = List.empty[TreeNode]
    def go(node: TreeNode, start: Boolean): Unit = {
      if (start && node.left == null)
        xs = node :: xs
      else if (node == null)
        ()
      else {
        xs = node :: xs
        if (node.left != null)
          go(node.left, false)
        else
          go(node.right, false)
      }
    }
    go(root, true)
    xs.reverse
  }
  val right = {
    var xs = List.empty[TreeNode]
    def go(node: TreeNode, start: Boolean): Unit = {
      if (start && node.right == null)
        xs = node :: xs
      else if (node == null)
        ()
      else {
        xs = node :: xs
        if (node.right != null)
          go(node.right, false)
        else
          go(node.left, false)
      }
    }
    go(root, true)
    xs
  }
  val leaves = {
    var xs = List.empty[TreeNode]
    def go(node: TreeNode): Unit = {
      if (node == null)
        ()
      else if (node.left == null && node.right == null)
        xs = node :: xs
      else {
        go(node.right)
        go(node.left)
      }
    }
    go(root)
    xs
  }
  (left ++ leaves ++ right).distinct.map(_.value)
}

List(1, 2).view

val x = T(1, T(2, T(4), T(5, T(7), T(8))), T(3, T(6, T(9), T(10))))
val y = T(1, null, T(2, T(3), T(4)))

boundaryOfBinaryTree(y)

