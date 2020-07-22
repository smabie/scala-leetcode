/*
 * 538 Convert BST to Greater Tree
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

def convertBST2(root: TreeNode) = {
  var xs = List.empty[Int]

  def build(node: TreeNode): Unit = {
    if (node == null)
      ()
    else {
      build(node.right)
      xs = node.value :: xs
      build(node.left)
    }
  }
  build(root)
  val ht = xs.zip(xs.scanRight(0)( _ + _).take(xs.length)).toMap

  def repl(node: TreeNode): TreeNode = {
    if (node == null)
      null
    else
      new TreeNode(ht(node.value), repl(node.left), repl(node.right))
  }
  repl(root)
}

def convertBST(root: TreeNode): TreeNode = {
  var sum = 0
  def go(node: TreeNode): TreeNode = {
    if (node == null)
      null
    else {
      val r = go(node.right)
      val v = sum + node.value
      sum += node.value
      val l = go(node.left)
      new TreeNode(v, l, r)
    }
  }
  go(root)
}

val in = T(5, T(2), T(13))

convertBST(in).left.value