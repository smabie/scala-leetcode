/*
 * 206 Reverse Linked List
 */

class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x
 }

def reverseList(head: ListNode): ListNode = {
  def go(rev: ListNode, xs: ListNode): ListNode = {
    if (xs == null) rev
    else {
      val n = xs.next
      xs.next = rev
      go(xs, n)
    }
  }
  go(null, head)
}
