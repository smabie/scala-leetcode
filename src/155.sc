/*
 * Min Stack
 *
 * Design a stack that supports push, pop, top, and retrieving the minimum element in constant time.

    push(x) -- Push element x onto stack.
    pop() -- Removes the element on top of the stack.
    top() -- Get the top element.
    getMin() -- Retrieve the minimum element in the stack.

 */

class MinStack() {
  var stk = List.empty[(Int, Int)]

  def push(x: Int): Unit = {
    stk = stk.headOption.fold(x -> x :: stk) { case (min, _) =>
      math.min(min, x) -> x :: stk
    }
  }

  def pop() {
    stk = stk.tail
  }

  def top(): Int = {
    stk.head._2
  }

  def getMin(): Int = {
    stk.head._1
  }
}

val mstk = new MinStack()

mstk.push(-2)
mstk.push(0)
mstk.push(-3)
mstk.getMin()
mstk.pop()
mstk.top()
mstk.getMin()