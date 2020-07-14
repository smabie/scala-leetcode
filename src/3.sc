/*
 * 3 Longest Substring Without Repeating Characters
 */

def lengthOfLongestSubstring(s: String): Int = {
  var start = 0
  var i = 0
  var len = 0
  val set = collection.mutable.LinkedHashSet.empty[Char]

  while (i < s.length) {
    if (!set.contains(s(i))) {
      set += s(i)
      i += 1
    } else {
      len = math.max(len, i - start)
      set -= s(start)
      start += 1
    }
  }
  math.max(len, i - start)
}


lengthOfLongestSubstring("ababbb")