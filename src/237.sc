/*
 * 237 Delete Node in a Linked List
 *
 * Write a function to delete a node (except the tail) in a singly linked list,
 * given only access to that node.
 */

class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x
 }

def deleteNode2(node: ListNode): Unit = {
  var p = node
  while (p != null) {
    p.x =  p.next.x
    if (p.next.next == null)
      p.next = null
    p = p.next
  }
}

def deleteNode(node: ListNode): Unit = {
  node.x = node.next.x
  node.next = node.next.next
}