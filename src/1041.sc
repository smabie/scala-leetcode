/*
 * 1041 Robot Bounded in Circle
 */


def isRobotBounded(instructions: String) = {
  var (x, y) = 0 -> 0
  var (i, j) = 0 -> 1
  instructions.foreach {
    case 'G' =>
      x += i
      y += j
    case 'L' =>
      val tmp = i
      i = j * (if (tmp == 0) -1 else 1)
      j = tmp
    case 'R' =>
      val tmp = i
      i = j
      j = tmp * (if (j == 0) -1 else 1)
  }

  (x == 0 && y == 0) || i != 0 || j != 1
}

isRobotBounded("LRRRL")



