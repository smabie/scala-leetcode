/*
 * 863 All Nodes Distance K in Binary Tree
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

def distanceK(root: TreeNode, target: TreeNode, K: Int): List[Int] = {

}

val x = T(3, T(5, T(6), T(2, T(7), T(4))), T(1, T(0), T(8)))

