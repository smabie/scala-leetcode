/*
 * 1490 Clone N-ary Tree
 */

class Node(var _value: Int) {
  var value: Int = _value
  var children: List[Node] = List()
}


def cloneTree(root: Node): Node = {
  def go(node: Node): Node = {
    if (node == null)
      null
    else {
      val n = new Node(node.value)
      n.children = node.children.map(go)
      n
    }
  }
  go(root)
}
