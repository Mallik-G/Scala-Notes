import math.abs

object execrise {
    def product(f: Int => Int)(a: Int, b: Int): Int =
        if(a > b) 1
        else f(a) * product(f)(a + 1, b)
    product(x => x * x)(3, 7)

    def fact(n: Int) = product(x => x)(1, n)

    fact(5)

    def mapReduce(f:Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
        if (a > b) zero
        else combine(f(a), mapReduce(f, combine, zero)(a+1, b))

    val tolerance = 0.0001
    def isCloseEnough(x: Double, y: Double): Boolean =
        abs((x - y) / x) / x < tolerance

    def fixedPoint(f: Double => Double)(firstGuess: Double) = {
        def iterate(guess: Double): Double = {
            val next = f(guess)
            if (isCloseEnough(guess, next)) next
            else iterate(next)
        }
        iterate(firstGuess)
    }

    def sqrt(x: Double) = fixedPoint(y => ( y + x / y ) / 2)(1)
    sqrt(2)
}