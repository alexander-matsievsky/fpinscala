package chapter3

sealed trait List[+A] {
  def toStdList: scala.collection.immutable.List[A] = this match {
    case Cons(h, t) => h :: t.toStdList
    case Nil => scala.collection.immutable.List()
  }
}

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  @scala.annotation.tailrec
  def all(bs: List[Boolean]): Boolean = bs match {
    case Cons(h, t) => if (!h) false else all(t)
    case _ => true
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def concat[A](las: List[List[A]]): List[A] =
    wFoldLeft.foldRight(las, Nil: List[A])(append)

  def doubleToString(ds: List[Double]): List[String] =
    wFoldLeft.foldRight(ds, Nil: List[String]) { (a, b) => Cons(a.toString, b) }

  @scala.annotation.tailrec
  def drop[A](as: List[A], i: Int): List[A] = as match {
    case Cons(_, t) if i > 0 => drop(t, i - 1)
    case `as` => as
  }

  @scala.annotation.tailrec
  def dropWhile[A](as: List[A], p: A => Boolean): List[A] = as match {
    case Cons(h, t) if p(h) => dropWhile(t, p)
    case `as` => as
  }

  def filter[A](as: List[A])(p: A => Boolean): List[A] =
    wFoldLeft.foldRight(as, Nil: List[A]) { (a, b) => if (p(a)) Cons(a, b) else b }

  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
    case Nil => z
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Cons(h, t) => f(h, foldRight(t, z)(f))
    case Nil => z
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @scala.annotation.tailrec
    def loop(lSup: Int, lSub: Int, sup: List[A], sub: List[A]): Boolean = sup match {
      case l@Cons(_, t) =>
        if (lSup < lSub) false
        else if (all(zipWith(l, sub) { case (a1, a2) => a1 == a2 })) true
        else loop(lSup - 1, lSub, t, sub)
      case Nil => lSub == 0
    }

    loop(length(sup), length(sub), sup, sub)
  }

  def increment(is: List[Int]): List[Int] =
    wFoldLeft.foldRight(is, Nil: List[Int]) { (a, b) => Cons(a + 1, b) }

  def init[A](as: List[A]): List[A] = as match {
    case Cons(_, Nil) | Nil => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](as: List[A]): Int =
    foldRight(as, 0) { (_, z) => z + 1 }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def product2(ds: List[Double]): Double =
    foldRight(ds, 1d)(_ * _)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A]) { (r, a) => Cons(a, r) }

  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Cons(_, t) => Cons(a, t)
    case Nil => List(a)
  }

  @scala.annotation.tailrec
  def some[A](as: List[A])(p: A => Boolean): Boolean = as match {
    case Cons(h, t) => p(h) || some(t)(p)
    case Nil => false
  }

  def map[A, B](as: List[A])(f: A => B): List[B] =
    wFoldLeft.foldRight(as, Nil: List[B]) { (a, b) => Cons(f(a), b) }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    wFoldLeft.foldRight(as, Nil: List[B]) { (a, b) => List.append(f(a), b) }

  def append[A](as1: List[A], as2: List[A]): List[A] =
    wFoldLeft.foldRight(as1, as2)(Cons(_, _))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def sum2(ints: List[Int]): Int =
    foldRight(ints, 0)(_ + _)

  def tail[A](as: List[A]): List[A] = as match {
    case Cons(_, t) => t
    case Nil => Nil
  }

  def toString[A](as: List[A]): String =
    as.toStdList.toString

  def zipSum(is1: List[Int], is2: List[Int]): List[Int] = {
    @scala.annotation.tailrec
    def loop(is: List[Int], is1: List[Int], is2: List[Int]): List[Int] = (is1, is2) match {
      case (Nil, _) | (_, Nil) => is
      case (Cons(h1, t1), Cons(h2, t2)) => loop(Cons(h1 + h2, is), t1, t2)
    }

    reverse(loop(Nil, is1, is2))
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    @scala.annotation.tailrec
    def loop(cs: List[C], as: List[A], bs: List[B]): List[C] = (as, bs) match {
      case (Nil, _) | (_, Nil) => cs
      case (Cons(a, at), Cons(b, bt)) => loop(Cons(f(a, b), cs), at, bt)
    }

    reverse(loop(Nil, as, bs))
  }

  object wFlatMap {
    def filter[A](as: List[A])(p: A => Boolean): List[A] =
      flatMap(as) { a => if (p(a)) Cons(a, Nil) else Nil }
  }

  object wFoldLeft {
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(as, identity[B] _) { (z, a) => z compose (f(a, _)) }(z)

    def length[A](as: List[A]): Int =
      foldLeft(as, 0) { (z, _) => z + 1 }

    def product(as: List[Double]): Double =
      foldLeft(as, 1d)(_ * _)

    def sum(as: List[Int]): Int =
      foldLeft(as, 0)(_ + _)
  }

  object wFoldRight {
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
      foldRight(as, identity[B] _) { (a, z) => z compose (f(_, a)) }(z)
  }

}