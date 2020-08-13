/*
 * 449 Serialize and Deserialize BST
 */

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
 }

object T {
  def apply(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null): TreeNode = {
    new TreeNode(_value, _left, _right)
  }
}
class Codec {
  // Encodes a list of strings to a single string.
  def serialize(root: TreeNode): String = {
    def go(node: TreeNode): String = {
      if (node == null)
        ""
      else
        s"${node.value},${go(node.left)},${go(node.right)}"
    }
    go(root)
  }
  // Decodes a single string to a list of strings.
  def deserialize(s: String): TreeNode = {
    val xs = s.split(',').flatMap { s =>
      if (s == "")
        None
      else
        Some(s.toInt)
    }

    def go(arr: Array[Int]): TreeNode = {
      if (arr.isEmpty)
        null
      else {
        val v = arr(0)
        val (xs, ys) = arr.tail.span(_ < v)
        new TreeNode(v, go(xs), go(ys))
      }
    }
    go(xs)
  }
}

val x = T(5, T(3, T(1), T(4)), T(6))

val codec = new Codec()


val s = codec.serialize(x)

codec.deserialize(s)