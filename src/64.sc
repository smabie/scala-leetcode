/*
 * 64 Minimum Path Sum
 */

def minPathSum2(grid: Array[Array[Int]]): Int = {
  val ydim = grid.length
  val xdim = grid(0).length

  println(s"$ydim $xdim")
  println()
  def go(y: Int, x: Int): Int = {
    println(s"$y $x")
    if (y + 1 == ydim && x + 1 == xdim)
      grid(y)(x)
    else if (x + 1 < xdim && y + 1 < ydim) {
      grid(y)(x) + math.min(go(y + 1, x), go(y, x + 1))
    } else if (x + 1 == xdim) {
      grid(y)(x) + go(y + 1, x)
    } else {
      grid(y)(x) + go(y, x + 1)
    }
  }

  go(0, 0)
}

def minPathSum(grid: Array[Array[Int]]) = {
  val ydim = grid.length
  val xdim = grid(0).length

  val mat = Array.fill(ydim)(Array.fill(xdim)(0))

  def get(y: Int, x: Int) = {
    if (y >= ydim || x >= xdim) Int.MaxValue
    else mat(y)(x)
  }

  var i = 0
  while (i < xdim) {
    var y = ydim - 1
    var x = xdim - i - 1

    while (x < xdim && y >= 0) {

      val a = get(y + 1, x)
      val b = get(y, x + 1)

      if (a == b && a == Int.MaxValue)
        mat(y)(x) = grid(y)(x)
      else
        mat(y)(x) = grid(y)(x) + math.min(a, b)

      x += 1
      y -= 1
    }

    i += 1
  }

  i = 0
  while (i < ydim) {
    var y = ydim - 2 - i
    var x = 0

    while (y >= 0 && x < xdim) {
      val a = get(y + 1, x)
      val b = get(y, x + 1)
      if (a == b && a == Int.MaxValue)
        mat(y)(x) = grid(y)(x)
      else
        mat(y)(x) = grid(y)(x) + math.min(a, b)

      x += 1
      y -= 1
    }

    i += 1
  }


  mat(0)(0)
}

val grid = Array(
  Array(1, 3, 1),
  Array(1, 5, 1),
  Array(4, 2, 1)
)

minPathSum(grid)

/*
1 2 3 4
3 4 5 6
1 5 2 0
2 3 4 1

1 1
2 2
3 3
4 4
5 3
6 2
7 1

1 2 2 2 2 2 2 1

1 2 3
1 2 3
2 1 1

3 2 1
1 2 3
5 3 1
1 2 3
3 2 1

1 2 3 3 3 2 1

1 1
2 2
3 3
4 3
5 3
6 2
7 1

1 1
2 2
3 3
4 3
5 2
6 1

1 1
2 2
3 1

1 2
3 1

y + (x - 1)

 */