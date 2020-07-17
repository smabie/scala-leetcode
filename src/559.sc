/*
 * 559 maximum depth of n-ary tree
 */

class Node(var _value: Int) {
  var value: Int = _value
  var children: List[Node] = List()
}

def maxDepth(root: Node): Int = {
  def go(node: Node): Int = {
    if (node == null)
      0
    else if (node.children.isEmpty)
      0
    else {
      node.children.map(n => 1 + go(n)).max
    }
  }
  go(root)
}


