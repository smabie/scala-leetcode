/*
 * 700 Search in a Binary Search Tree
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

def searchBST2(root: TreeNode, `val`: Int): TreeNode = {
  val x = `val`
  def go(node: TreeNode): TreeNode = {
    if (node == null) {
      null
    } else if (node.value == x)
        node
    else {
      val a = go(node.left)
      val b = go(node.right)
      if (a == null && b == null)
        null
      else if (a != null && b == null)
        a
      else if (b != null && a == null)
        b
      else                      /* not reached */
        a
    }
  }
  go(root)
}

def searchBST(root: TreeNode, `val`: Int): TreeNode = {
  val x = `val`
  def go(node: TreeNode): TreeNode = {
    if (node == null)
      null
    else if (node.value == x)
        node
    else
      Option(go(node.left)).orElse(Option(go(node.right))).orNull
  }
  go(root)
}

val in = T(4, T(2, T(1), T(3)), T(7))

searchBST(in, 2).value