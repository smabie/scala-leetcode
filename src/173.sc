/*
 * 173 Binary Search Tree Iterator
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

class BSTIterator(_root: TreeNode) {
  val iter = {
    var xs = List.empty[Int]

    def go(node: TreeNode): Unit = {
      if (node == null)
        ()
      else {
        go(node.right)
        xs = node.value :: xs
        go(node.left)
      }
    }

    go(_root)
    xs
  }.iterator
  def next() = iter.next()
  def hasNext() = iter.hasNext
}

val x = T(7, T(3), T(15, T(9), T(20)))

val iter = new BSTIterator(x)

iter.next()
iter.next()
iter.next()