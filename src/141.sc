/*
 * 141 cycle in linked list
 */

class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x
}

 def hasCycle(head: ListNode): Boolean = {
   var x = head
   var y = head

   while (y != null) {
     x = x.next
     if (x == null || x.next == null || y.next == null)
       return false
     y = y.next.next
     if (x == y)
       return true
   }
   false
 }