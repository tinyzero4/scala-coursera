object rationals {
  val x = new Rational(1,3)
  val y = new Rational(5,7)
  val z = new Rational(3,2)

  def gcd(a:Int, b:Int):Int = if (b == 0) a else gcd(b, a % b)

  x.sub(y).sub(z)
  gcd(21, 9)
  gcd(9, 21)

  y.add(y)
}



class Rational(x:Int, y:Int) {

  private def gcd(a:Int, b:Int):Int = if (b == 0) a else gcd(b, a % b)
  private def g = gcd(x, y)
  def number = x / g
  def denom = y / g

  def neg = new Rational(
    -number,
    denom
  )

  def add(that: Rational) = new Rational (
    number * that.denom + that.number * denom,
    denom * that.denom
  )

  def sub(that: Rational) =  add(that.neg)

  override def toString = number + "/" + denom

}
