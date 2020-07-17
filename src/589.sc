/*
 * 589 n-ary tree preorder traversal
 */

class Node(var _value: Int) {
   var value: Int = _value
   var children: List[Node] = List()
 }

def preorder(root: Node): List[Int] = {
  def go(node: Node): List[Int] = {
    if (node == null)
      Nil
    else
      node.value :: node.children.flatMap(go)
  }
  go(root)
}