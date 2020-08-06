/*
 * 1315 Sum of Nodes with Even-Valued Grandparent
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

def sumEvenGrandparent2(root: TreeNode): Int = {
  var sum = 0
  def go(node: TreeNode, parents: List[Int]): Unit = {
    if (node == null)
      ()
    else {
      if (parents.length > 1 && parents(1) % 2 == 0)
          sum += node.value
      go(node.left, node.value :: parents)
      go(node.right, node.value :: parents)
    }
  }
  go(root, Nil)
  sum
}

def sumEvenGrandparent(root: TreeNode): Int = {
  var sum = 0
  def go(node: TreeNode, q: collection.mutable.Queue[Int]): Unit = {
    if (node == null)
      ()
    else {
      if (q.length > 1 && q.dequeue % 2 == 0)
          sum += node.value
      go(node.left, q.clone.enqueue(node.value))
      go(node.right, q.clone.enqueue(node.value))
    }
  }
  go(root, collection.mutable.Queue.empty[Int])
  sum
}

val x = T(6, T(7, T(2, T(9)), T(7, T(1), T(4))), T(8, T(1), T(3, null, T(5))))


sumEvenGrandparent(x)