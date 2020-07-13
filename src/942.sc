/*
 * 942 DI String Match
 */

import scala.collection.mutable.TreeSet

def diStringMatch2(S: String): Array[Int] = {
  val xs = S.foldLeft(List.empty[(Int, Char)]) {
    case (Nil, ch) =>  (1, ch) :: Nil
    case (ys@(x, ch) :: xs, nch) =>
      if (ch == nch) (x+1, ch) :: xs
      else (1, nch) :: ys
  }.reverse
  val len = S.length

  val set = TreeSet.from(0 to len)
  val init = if (xs.head._2 == 'I') 0 else len
  set -= init

  (init :: xs.flatMap { case (n, i) =>
      (1 to n).map { _ =>
        val x = if (i == 'I') set.max else set.min
        set -= x
        x
      }.reverse.toList
  }).toArray
}

def diStringMatch(S: String) = {
  val len = S.length
  val ret = new Array[Int](len+1)
  var (max, min) = len -> 0

  var i = 0
  while (i < len) {
    if (S(i) == 'D') {
      ret(i) = max
      max -= 1
    } else {
      ret(i) = min
      min += 1
    }
    i += 1
  }
  ret(len) = min
  ret
}

diStringMatch2("DDDIDIID")
diStringMatch("DDDIDIID")
