/*
 * 590 N-ary Tree Postorder Traversal
 */

 class Node(var _value: Int) {
   var value: Int = _value
   var children: List[Node] = List()
 }

def postorder(root: Node): List[Int] = {
  def go(node: Node): List[Int] = {
    if (node == null)
      Nil
    else
      node.children.flatMap(go) ++ List(node.value)
  }
  go(root)
}

def postorderiter(root: Node): List[Int] = {
  var xs = List.empty[Int]


}