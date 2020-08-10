/*
 * 98 Validate Binary Search Tree
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

def isValidBST2(root: TreeNode): Boolean = {
  var xs = List.empty[Int]
  def go(node: TreeNode): Unit = {
    if (node == null)
      ()
    else {
      go(node.right)
      xs = node.value :: xs
      go(node.left)
    }
  }
  go(root)
  xs.foldLeft(true -> Int.MinValue) { case ((b, v), n) =>
    if (!b)
      b -> 0
    else if (v <= n)
      true -> n
    else
      false -> 0
  }._1
}

def isValidBST(root: TreeNode): Boolean = {
  var min = Long.MaxValue
  var valid = true

  def go(node: TreeNode): Unit = {
    if (!valid || node == null)
      ()
    else {
      go(node.right)
      if (node.value < min)
        min = node.value
      else
      valid = false
      go(node.left)
    }
  }
  go(root)
  valid
}


val x = T(2, T(1), T(3))
val y = T(5, T(1), T(4, T(3), T(6)))
isValidBST2(x)