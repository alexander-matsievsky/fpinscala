package chapter2

object Chapter2 {

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  def fib(n: Int): Int = {
    @scala.annotation.tailrec
    def loop(i: Int, i1: Int, i2: Int): Int =
      if (i <= 0) i1
      else loop(i - 1, i2, i1 + i2)

    loop(n, 0, 1)
  }

  def isSorted[A](as: List[A], ordered: (A, A) => Boolean): Boolean = {
    @scala.annotation.tailrec
    def loop(i: Int): Boolean =
      if (i <= 0) true
      else if (!ordered(as(i - 1), as(i))) false
      else loop(i - 1)

    loop(as.length - 1)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  object ref {
    def fib(n: Int): Int = {
      var (i, i1, i2) = (n, 0, 1)
      while (i > 0) {
        i1 = i1 + i2
        i2 = i1 - i2
        i -= 1
      }
      i1
    }
  }

}
