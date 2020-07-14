/*
 * 146 LRU Cache
 */

class LRUCache(_capacity: Int) {
  val ht = collection.mutable.Map.empty[Int, Int]
  var n = 0
  var lru = Int.MinValue

  collection.mutable.LinkedHashMap
  def get(key: Int): Int = {

  }

  def put(key: Int, value: Int) {
    if (n < _capacity) {
      ht(key) = value
      lru = key
      n += 1
    }


  }

}
