/*
 * 289 Game of Life
 */

def gameOfLife(board: Array[Array[Int]]) = {
  val ydim = board.length
  val xdim = board(0).length

  def get(y: Int, x: Int) = {
    if (x >= 0 && y >= 0 && y < ydim && x < xdim) {
      board(y)(x) & 1
    } else
      0
  }

  def n(y: Int, x: Int): Int = {
    get(y, x - 1) +
      get(y, x + 1) +
      get(y + 1, x) +
      get(y - 1, x) +
      get(y - 1, x + 1) +
      get(y + 1, x + 1) +
      get(y - 1, x - 1) +
      get(y + 1, x - 1)
  }

  var i = 0
  var j = 0
  while (j < ydim) {
    i = 0
    while (i < xdim) {
      n(j, i) match {
        case 2 | 3 if get(j, i) == 1 => board(j)(i) |= 2
        case 3 => board(j)(i) |= 2
        case 0 | 1 => board(j)(i) &= ~2
        case 4 | 5 | 6 | 7 | 8 => board(j)(i) &= ~2
        case _ =>
      }
      i += 1
    }
    j += 1
  }

  j = 0
  i = 0
  while (j < ydim) {
    i = 0
    while (i < xdim) {
      board(j)(i) >>= 1
      i += 1
    }
    j += 1
  }
}

gameOfLife(Array(Array(0, 1, 0), Array(0, 0, 1), Array(1, 1, 1), Array(0, 0, 0)))
