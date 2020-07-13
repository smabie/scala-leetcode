/*
 * 20 valid parenthesis
 */

def isValid(s: String): Boolean = {
  val stk = scala.collection.mutable.Stack.empty[Char]

  var i = 0
  while (i < s.length) {
    s(i) match {
      case x@('(' | '[' | '{') => stk.push(x)
      case x =>
        stk.headOption.fold {
          return false
        } {
          case '(' if x == ')' => stk.pop
          case '[' if x == ']' => stk.pop
          case '{' if x == '}' => stk.pop
          case _ => return false
        }
    }
    i += 1
  }
  stk.isEmpty
}



isValid("{[]}")