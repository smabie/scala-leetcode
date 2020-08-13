/*
 * 285 Inorder Successor in BST
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

def inorderSuccessor2(root: TreeNode, p: TreeNode): TreeNode = {
  var xs = List.empty[TreeNode]

  def go(node: TreeNode): Unit = {
    if (node == null)
      ()
    else {
      go(node.right)
      xs = node :: xs
      go(node.left)
    }
  }
  go(root)

  xs.zipWithIndex.find(_._1 == p).fold(null.asInstanceOf[TreeNode]) { case (_, ix) =>
    if (ix + 1 == xs.length)
      null
    else
      xs(ix + 1)
  }
}

def inorderSuccessor(root: TreeNode, p: TreeNode): TreeNode = {
  def go(node: TreeNode, parent: TreeNode): TreeNode = {
    if (node == null)
      null
    else {
      val l = go(node.left, node)
      val r = go(node.right, node)

      List(l, r, node).filter { tn =>
        tn != null && tn.value > p.value
      }.minByOption(_.value).orNull
    }
  }
  go(root, null)
}

val x = T(2, T(1), T(3))
val y = T(5, T(3, T(2, T(1)), T(4)), T(6))

inorderSuccessor(y, y.left.right).value