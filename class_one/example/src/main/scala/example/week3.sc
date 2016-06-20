

object rationals {
    val x = new Rational(1, 3)
    val y = new Rational(5, 7)
    val z = new Rational(3, 2)
    new Rational(-1, 3)
    y + y

    class Rational(x: Int, y: Int) {
        require(y > 0, "denominator must be positive.")
        private def gcd(a:Int, b:Int) : Int = if (b == 0) a else gcd(b , a%b)

        private val g = gcd(x, y)
        def numer = x / g
        def denom = y / g

        def + (that: Rational) =
            new Rational(
                numer * that.denom + denom * that.numer,
                denom * that.denom)
        def neg: Rational = new Rational(-numer, denom)

        def - (that: Rational) =
            this + that.neg

        override def toString = numer + "/" + denom
    }
}
