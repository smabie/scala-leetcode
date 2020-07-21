/*
 * 637 Average Levels in Binary Tree
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

def averageOfLevels(root: TreeNode): Array[Double] = {
  case class Data(n: Int, sum: Long)
  val ht = collection.mutable.SortedMap.empty[Int, Data]

  def go(level: Int, node: TreeNode): Unit = {
    if (node == null)
      ()
    else {
      ht.get(level).fold {
        ht(level) = Data(1, node.value)
      } { case Data(n, sum) =>
        ht(level) = Data(n + 1, sum + node.value)
      }
      go(1 + level, node.left)
      go(1 + level, node.right)
    }
  }
  go(0, root)
  ht.values.map { case Data(n, sum) => sum.toDouble / n }.toArray
}

val in = T(3, T(9), T(20, T(15), T(7)))

averageOfLevels(in)