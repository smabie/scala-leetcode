/*
 * 1485 Clone Binary Tree With Random Pointer
 */

class Node(var _value: Int, _left: Node = null, _right: Node = null, _random: Node = null) {
  var value: Int = _value
  var left: Node = _left
  var right: Node = _right
  var random: Node = _random
}

class NodeCopy(var _value: Int, _left: NodeCopy = null, _right: NodeCopy = null, _random: NodeCopy = null) {
  var value: Int = _value
  var left: NodeCopy = _left
  var right: NodeCopy = _right
  var random: NodeCopy = _random
}

def copyRandomBinaryTree(root: Node): NodeCopy = {
  val ht = collection.mutable.HashMap.empty[Node, NodeCopy]

  def go(node: Node): NodeCopy = {
    if (node == null)
      null
    else {
      val n = new NodeCopy(node.value, go(node.left), go(node.right))
      ht(node) = n
      n
    }
  }
  val n = go(root)

  def fix(x: Node, y: NodeCopy): Unit = {
    if (x == null && y == null)
      ()
    else {
      y.random = if (x.random == null) null else ht(x.random)
      fix(x.left, y.left)
      fix(x.right, y.right)
    }
  }
  fix(root, n)
  n
}
