/*
 * 387 First Unique Character in a String
 *
 * Given a string, find the first non-repeating character
 * in it and return its index. If it doesn't exist, return -1.
 */

def firstUniqChar(s: String): Int = {
  val arr = Array.fill(26)(0 -> 0)

  s.zipWithIndex.foreach { case (ch, ix) =>
    val (_, n) = arr(ch - 'a')
    arr(ch - 'a') = ix -> (n+1)
  }

  s.indexWhere { ch => arr(ch - 'a')._2 == 1 }
}

firstUniqChar("loveleetcode")