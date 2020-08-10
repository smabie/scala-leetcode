/*
 * 987 Vertical Order Traversal of a Binary Tree
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

def verticalTraversal(root: TreeNode): List[List[Int]] = {
  val ht = collection.mutable.HashMap.empty[Int, List[(Int, Int)]]
  def go(node: TreeNode, x: Int, y: Int): Unit = {
    if (node == null)
      ()
    else {
      ht(x) = ht.get(x).fold(List(y -> node.value))((y -> node.value) :: _)
      go(node.left, x - 1, y - 1)
      go(node.right, x + 1, y - 1)
    }
  }
  go(root, 0, 0)
  ht.toList.sortBy(_._1).map { case (_, xs) =>
    xs.sortWith { case ((y, a), (yy, b)) =>
      println(s"$y $a $yy $b")
      if (y == yy)
        a <= b
      else
        y >= yy
    }.map(_._2)
  }
}

val x = T(3, T(9), T(20, T(15), T(7)))

verticalTraversal(x)