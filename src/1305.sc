/*
 * 1305 All Elements in Two Binary Search Trees
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

def getAllElements2(root1: TreeNode, root2: TreeNode): List[Int] = {
  var xs = List.empty[Int]

  def add(node: TreeNode): Unit = {
    if (node == null)
      ()
    else {
      xs = node.value :: xs
      add(node.left)
      add(node.right)
    }
  }
  add(root1)
  add(root2)
  xs.sorted
}

def getAllElements(root1: TreeNode, root2: TreeNode): List[Int] = {
  def add(node: TreeNode): List[Int] = {
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
    go(node)
    xs
  }

  def merge(xs: List[Int], ys: List[Int]): List[Int] = {
    xs -> ys match {
      case (Nil, yy) => yy
      case (xx, Nil) => xx
      case (xx @ x :: xs, yy @ y :: ys) =>
        if (x < y)
          x :: merge(xs, yy)
        else
          y :: merge(xx, ys)
    }
  }
  merge(add(root1), add(root2))
}


//val x = T(5, T(2), T(7))
//getAllElements(x, x)


