/*
 * 215 Kth Largest Element in Array
 */

def findKthLargest2(nums: Array[Int], k: Int): Int = {
  nums.sorted.apply(nums.length - k)
}

def findKthLargest(nums: Array[Int], k: Int) = {
  collection.mutable.PriorityQueue.from(nums).dequeueAll.apply(k - 1)
}

findKthLargest(Array(3, 2, 3, 1, 2, 4, 5, 5, 6), 4)