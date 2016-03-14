val x = new Rational(1,3)
x.numer
x.denom

val y = new Rational(5,7)
y + x

val z = new Rational(3,2)
x - y - z
x max y
new Rational(2,4)


class Rational(x: Int, y: Int) {
  val g = gcd(x,y)
  def numer = x/g
  def denom = y/g

  def +(that: Rational): Rational = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )
  }

  def unary_- = new Rational(-numer, denom)

  def -(that: Rational): Rational = this + -that

  def <(that: Rational) = numer * that.denom < that.numer * denom
  def max(that: Rational) = if (this < that) that else this

  def gcd(a: Int, b: Int): Int =
    if (b==0) a else gcd(b, a % b)

  override def toString = numer + "/" + denom
}