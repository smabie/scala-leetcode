/*
 * 426 Convert Binary Search Tree to Sorted Doubly Linked List
 */

class Node(_value: Int = 0, _left: Node = null, _right: Node = null) {
  var value: Int = _value
  var left  = _left
  var right = _right
 }

object T {
  def apply(_value: Int = 0, _left: Node = null, _right: Node = null): Node = {
    new Node(_value, _left, _right)
  }
}

def treeToDoublyList(root: Node): Node = {
  var xs = List.empty[Node]

  def go(node: Node): Unit = {
    if (node == null)
      ()
    else {
      go(node.right)
      xs = node :: xs
      go(node.left)
    }
  }
  go(root)
  if (xs.isEmpty)
    null
  else if (xs.length == 1)
    xs.head
  else {
    val head = xs.head
    val tail = xs.last
    xs.sliding(2).foreach {
      case x :: y :: Nil =>
        x.right = y
        y.left = x
      case _ =>
    }
    head.left = tail
    tail.right = head
    head
  }
}

val x = T(4, T(2, T(1), T(3)), T(5))
val y = T(2, T(1), T(3))


