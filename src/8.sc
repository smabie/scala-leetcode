/*
 * 8 String to Integer
 */

def myAtoi(str: String) = {
  val s = str.dropWhile(_ == ' ')

  val st = s.headOption match {
    case Some(c @ ('+' | '-')) =>
      c.toString ++ s.tail.takeWhile(_.isDigit)
    case _ =>
      s.takeWhile(_.isDigit)
  }
  val n = st.foldRight(BigInt(0) -> BigInt(1)) { case (ch, (n, pw)) =>
    ch-> pw match {
      case '+' -> x if x == 0 => BigInt(0) -> x
      case '-' -> x if x == 0 => BigInt(0) -> x
      case '-' -> _ => -1 * n -> BigInt(0)
      case '+' -> _ => n -> BigInt(0)
      case c -> x =>
        (n + x * (c - '0')) -> (10 * x)
    }
  }._1

  if (n > Int.MaxValue) Int.MaxValue
  else if (n < Int.MinValue) Int.MinValue
  else n.toInt
}

//myAtoi("10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000522545459"
//)

