/*
 * 1261 Find Elements in a Contaminated Binary Tree
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

class FindElements(_root: TreeNode) {
  def fix(v: Int, node: TreeNode): Unit = {
    if (node == null)
      ()
    else {
      node.value = v
      fix(2 * v + 1, node.left)
      fix(2 * v + 2, node.right)
    }
  }

  fix(0, _root)

  def find(target: Int): Boolean = {
    var found = false
    def go(node: TreeNode): Unit = {
      if (node == null)
        ()
      else if (node.value == target)
          found = true
      else {
          go(node.left)
          go(node.right)
      }
    }
    go(_root)
    found
  }
}