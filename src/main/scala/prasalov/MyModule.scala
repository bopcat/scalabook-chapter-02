package prasalov

import scala.annotation.tailrec

/**
 * Created by kirillp on 10.03.16.
 */
object MyModule {

  def abs(n: Int): Int =
    if (n > 0) n
    else -n

  private def formatAbs(n: Int): String = {
    val msg = "Number %d has abs=%d"
    msg.format(n, abs(n))
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit =
    //println(fib(-1))
    //println(formatResult("abs", -42, abs))
    println(isSorted(Array(), (x: Int, y: Int) => x <= y))

  def factorial(n: Int): Int = {

    @tailrec def _factorial(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else _factorial(n - 1, acc * n)

    _factorial(n, 1)
  }

  def fib(n: Int): Int = {

    @tailrec def _fib(cur: Int, prev: Int, ind: Int): Int =
      if (ind > n) prev
      else if (ind == n) cur
      else _fib(cur + prev, cur, ind + 1)

    if (n < 0) throw new Exception("Fibonacci number's index should be 0 or greater!")
    else _fib(1, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec def _isSorted[A](as: Array[A], ordered: (A, A) => Boolean, ind: Int): Boolean = {
      if (ind >= as.length - 1) true
      else if (!ordered(as(ind), as(ind + 1))) false
      else _isSorted(as, ordered, ind + 1)
    }
    _isSorted(as, ordered, 0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    def b2c(a: A, f: (A, B) => C): B => C =
      b => f(a, b)
    a => b2c(a, f)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}
