/*
 * 501 Find Mode in Binary Search Tree
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


 def findMode(root: TreeNode): Array[Int] = {
  val ht = collection.mutable.HashMap.empty[Int, Int]
  def go(node: TreeNode): Unit = {
    if (node == null)
      ()
    else {
      ht.get(node.value).fold {
        ht += node.value -> 1
      } { n =>
        ht += node.value -> (n + 1)
      }
      go(node.left)
      go(node.right)
    }
  }
  go(root)
  val xs = ht.toList.sortBy(_._2)(Ordering.Int.reverse)
  xs.headOption.fold {
    Array.empty[Int]
  } { case (_, h) =>
    xs.takeWhile { case (_, n) => n == h }.map(_._1).toArray
  }
}

val x = T(1, null, T(2, T(2)))

findMode(x)