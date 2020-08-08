/*
 * 366 Find Leaves of Binary Tree
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

def findLeaves(root: TreeNode): List[List[Int]] = {
  def rem(node: TreeNode): (TreeNode, List[Int]) = {
    var xs = List.empty[Int]
    def go(node: TreeNode): TreeNode = {
      if (node == null)
        null
      else if (node.left == null && node.right == null) {
        xs = node.value :: xs
        null
      } else new TreeNode(node.value, go(node.left), go(node.right))
    }
    go(node) -> xs
  }
  def find(node: TreeNode): List[List[Int]] = {
    if (node == null)
      Nil
    else {
      val (t, xs) = rem(node)
      xs :: find(t)
    }
  }
  find(root)
}

val x = T(1, T(2, T(4), T(5)), T(3))

findLeaves(x)