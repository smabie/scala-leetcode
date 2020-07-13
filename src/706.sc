/*
 * 706 Design HashMap
 */

class MyHashMap() {

  val arr = Array.fill(1000001)(-1)

  def put(key: Int, value: Int) = {
    arr(key) = value
  }

  def get(key: Int): Int = {
    arr(key)
  }

  def remove(key: Int) = {
    arr(key) = -1
  }
}