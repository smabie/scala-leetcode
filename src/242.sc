/*
 * 252 Valid Anagram
 * Given two strings s and t , write a function to determine if t is an anagram of s.
 */

def isAnagram(s: String, t: String): Boolean = {
  s.sorted == t.sorted
}

def isAnagram2(s: String, t: String): Boolean = {
  val arr = new Array[Int](26)
  s.foreach { ch => arr(ch - 'a') += 1 }
  t.foreach { ch => arr(ch - 'a') -= 1 }
  arr.forall(_ == 0)
}

isAnagram2("a", "b")