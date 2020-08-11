/*
 * 327 Path Sum III
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

def pathSum(root: TreeNode, sum: Int): Int = {
  val paths = collection.mutable.Set.empty[List[TreeNode]]
  def go(node: TreeNode, path: List[TreeNode]): Unit = {
    if (node == null) {
      (1 to path.length).foreach { n =>
        path.sliding(n).foreach(paths += _)
      }
    } else {
      go(node.left, node :: path)
      go(node.right, node  :: path)
    }
  }
  go(root, List.empty[TreeNode])
  paths.view.map(_.foldLeft(0) { case (acc, x) => acc + x.value}).count(_ == sum)
}

val x = T(10, T(5, T(3, T(3), T(-2)), T(2, null, T(1))), T(-3, null, T(11)))

pathSum(x, 8)