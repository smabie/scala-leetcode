/*
 * 1104 Path in Zigzag Labelled Binary Tree
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

def pathInZigZagTree(label: Int) = {
  def parent(label: Int): Int = {
    val lvl = math.floor(math.log(label)/math.log(2))
    val x = math.pow(2, lvl).toInt
    x-(label-x)/2-1
  }
  def go(x: Int): List[Int] = {
    if (x == 1)
      List(1)
    else
      x :: go(parent(x))
  }
  go(label).reverse
}

pathInZigZagTree(13)