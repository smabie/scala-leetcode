/*
 * 146 LRU Cache
 */

class LRUCache(_capacity: Int) {
  val ht = collection.mutable.LinkedHashMap.empty[Int, Int]

  var sz = 0

  def get(key: Int): Int = {
    ht.get(key).fold {
      -1
    } { v =>
      ht.remove(key)
      ht += key -> v
      v
    }
  }

  def put(key: Int, value: Int) = {
    if (ht.contains(key)) {
      ht -= key
      ht += key -> value
    } else if (sz == _capacity) {
      ht -= ht.head._1
      ht += key -> value
    } else {
      ht += key -> value
      sz += 1
    }
    ht
  }
}

val cache = new LRUCache(2)

cache.get(2)
cache.put(2, 6)
cache.get(1)
cache.put(1, 5)
cache.put(1, 2)
cache.get(1)
cache.get(2)