/*
 * 722 Remove Comments
 */

def removeComments(source: Array[String]): List[String] = {
  sealed trait State
  case object Out extends State
  case object Slash extends State
  case object TwoSlash extends State
  case object SlashAsterick extends State
  case object Asterick extends State

  val st: State = Out
  source.foldLeft((List.empty[String], st, false)) { case ((xs, st, joinPrev), str) =>
    val buf = StringBuilder.newBuilder
    var i = 0

    var state = st

    while (i < str.length) {
      state match {
        case Out =>
          if (str(i) == '/' && i + 1 != str.length)
            state = Slash
          else
            buf.addOne(str(i))
        case Slash =>
          if (str(i) == '*')
            state = SlashAsterick
          else if (str(i) == '/')
            state = TwoSlash
          else {
            state = Out
            buf.addOne('/')
            buf.addOne(str(i))
          }
        case SlashAsterick =>
          if (str(i) == '*')
            state = Asterick
        case Asterick =>
          if (str(i) == '/')
            state = Out
          else if (str(i) != '*')
            state = SlashAsterick
        case TwoSlash =>
      }
      i += 1
    }
    val outJoinPrev = if (state == SlashAsterick) true else false
    val outStr = buf.toString
    val outState = if (state == TwoSlash) Out else state

    if (!outStr.isEmpty && joinPrev) {
      ((xs.head ++ outStr) :: xs.tail, outState, false)
    } else if (outStr.isEmpty) (xs, outState, outJoinPrev)
    else (outStr :: xs, outState, outJoinPrev)
  }._1.reverse
}
val in2 = Array("struct Node{", "    /*/ declare members;/**/", "    int size;", "    /**/int val;", "};")

val in3 = Array("a/*/b//*c","blank","d/*/e*//f")
val in = Array("void func(int k) {", "// this function does nothing /*", "   k = k*2/4;", "   k = k/2;*/", "}")
removeComments(in)

collection.mutable.LinkedHashMap