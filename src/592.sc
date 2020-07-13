/*
 * 592 Fraction Addition and Subtraction
 */

def fractionAddition(expression: String) = {
  case class Frac(num: Int, denom: Int)

  def frac(s: String) = {
    s.headOption match {
      case Some(sgn@('-' | '+')) =>
        val x = s.indexOf('+', 1)
        val y = s.indexOf('-', 1)

        val ix = if (y == -1 && x == -1) s.length
        else if (x == -1) y
        else if (y == -1) x
        else math.min(x, y)

        val frac = s.substring(1, ix)
        val Array(num, denom) = frac.split('/')

        Some(Frac((if (sgn == '-') -1 else 1) * num.toInt, denom.toInt) -> s.substring(ix))
      case _ => None
    }
  }

  def go(s: String): List[Frac] = {
    frac(s) match {
      case Some(fr -> str) => fr :: go(str)
      case None => Nil
    }
  }

  val fracs = go(if (expression.head != '-') "+" ++ expression else expression)

  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  
  val m = fracs.map(_.denom).product

  val Frac(num, denom) = fracs.map { case Frac(num, denom) =>
    val x = m / denom
    Frac(x * num, x * denom)
  }.reduce[Frac] { case (x, y) =>
    Frac(x.num + y.num, x.denom)
  }
  println(s"$num $denom")
  val g = gcd(math.abs(num), denom)

  val (x, y) = num/g -> denom/g

  s"$x/$y"
}

fractionAddition("5/3+1/3")

