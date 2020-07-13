/*
 * 524 Longest Word in Dictionary through Deleting
 */

def findLongestWord(s: String, d: List[String]): String = {
  d.foldLeft("") { case (ls, ns) =>
    val st = s.foldLeft(ns) { case (ns, ch) =>
      ns.headOption match {
        case Some(c) if c == ch => ns.tail
        case _ => ns
      }
    }
    
    if (!st.isEmpty) ls
    else if (ns.length > ls.length) ns
    else if (ns.length < ls.length) ls
    else if (ls < ns) ls
    else ns
  }
}

findLongestWord("bab", List("ba", "ab", "a", "b"))