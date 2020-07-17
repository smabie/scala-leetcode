/*
 * 897 increasing order search tree
 */


 class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
   var value: Int = _value
   var left: TreeNode = _left
   var right: TreeNode = _right
 }

def increasingBST(root: TreeNode): TreeNode = {
  var tr: TreeNode = null
  var cur: TreeNode = null

  def go(node: TreeNode): Unit = {
    if (node == null)
      ()
    else {
      go(node.left)
      if (tr == null) {
        tr = new TreeNode(node.value)
        cur = tr
      } else {
        cur.right = new TreeNode(node.value)
        cur = cur.right
      }
      go(node.right)
    }
  }
  go(root)
  tr
}

