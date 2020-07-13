/*
 * 49 Group Anagrams
 */

def groupAnagrams(strs: Array[String]) = {
  val ht = collection.mutable.Map.empty[String, List[String]]

  strs.foreach { s =>
    val sort = s.sorted
    ht.get(sort).fold {
      ht(sort) = List(s)
    } { xs =>
      ht(sort) = s :: xs
    }
  }
  ht.values.toList
}

groupAnagrams(Array("eat","tea", "tan", "ate", "nat", "bat"))