object exercise {

  def sum(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + product(f)(a + 1, b)

  def product(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, (x, y) => x * y, 1)(a, b)

  product(x => x * x)(3, 4)

  def factorial(n: Int): Int = product(x => x)(1, n)

  factorial(5)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, defVal: Int)(a: Int, b: Int): Int =
    if (a > b) defVal
    else combine(f(a), mapReduce(f, combine, defVal)(a + 1, b))

  mapReduce(x => x, (a, b) => a * b, 1)(1, 5)

  val tolerance = 0.0001

  import math.abs

  def isCloseEnough(x: Double, y: Double) = abs((x - y) / x) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def sqrt(x:Double) = {
    fixedPoint(averageDamp(y => x / y)) (1)
  }

  sqrt(2)


}