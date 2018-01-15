package chapter5

sealed trait Stream[+A] {
  def append[A2 >: A](as: => Stream[A2]): Stream[A2] =
    foldRight(as) { (a, as) => Stream.cons(a, as) }

  def drop(n: Int): Stream[A] = {
    @scala.annotation.tailrec
    def loop(as: Stream[A], n: Int): Stream[A] = as match {
      case Cons(_, t) if n > 0 => loop(t(), n - 1)
      case _ => as
    }

    loop(this, n)
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false) { (a, b) => p(a) || b }

  def filter(f: A => Boolean): Stream[A] =
    flatMap(a => if (f(a)) Stream.cons(a, Stream.empty) else Stream.empty)

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty: Stream[B]) { (a, bs) =>
      f(a).foldRight(bs) { (b, bs) => Stream.cons(b, bs) }
    }

  def foldRight[B](b: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(b)(f))
    case Empty => b
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true) { (a, b) => p(a) && b }

  def headOption: Option[A] = this match {
    case Cons(h, _) => Some(h())
    case Empty => None
  }

  def map[B](f: A => B): Stream[B] =
    flatMap(a => Stream.cons(f(a), Stream.empty))

  def scanRight[B](b: => B)(f: (A, B) => B): Stream[B] =
    foldRight((b, Stream(b))) { (a, tuple) =>
      tuple match {
        case (b_, bs) =>
          lazy val b2 = f(a, b_)
          (b2, Stream.cons(b2, bs))
      }
    }._2

  def startsWith[A2 >: A](as2: Stream[A2]): Boolean =
    Stream.wUnfold.zipAll(this, as2)
      .takeWhile { case (_, oa2) => oa2.isDefined }
      .forAll { case (oa1, oa2) => oa1 == oa2 }

  def tails: Stream[Stream[A]] =
    Stream(this) append Stream.unfold(this) {
      case Cons(_, t) => Some((t(), t()))
      case _ => None
    }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
    case _ => Stream.empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] = (this, bs) match {
    case (Cons(a, as), Cons(b, bs_)) => Stream.cons(f(a(), b()), as().zipWith(bs_())(f))
    case _ => Stream.empty
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val h = head
    lazy val t = tail
    Cons(() => h, () => t)
  }

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs: Stream[Int] =
    cons(0, cons(1, fibs.zipWith(fibs.drop(1))(_ + _)))

  def ones: Stream[Int] =
    cons(1, ones)

  def unfold[A, S](s: S)(f: S => Option[(A, S)]): Stream[A] =
    f(s).map { case (a, s_) => cons(a, unfold(s_)(f)) }.getOrElse(empty)

  def empty[A]: Stream[A] = Empty

  object wFoldRight {
    def headOption[A](as: Stream[A]): Option[A] =
      as.foldRight(None: Option[A]) { (a, _) => Some(a) }

    def takeWhile[A](as: Stream[A], p: A => Boolean): Stream[A] =
      as.foldRight(empty: Stream[A]) { (a, as) =>
        if (p(a)) cons(a, as)
        else empty
      }
  }

  object wUnfold {
    def constant[A](a: A): Stream[A] =
      unfold(a) { a => Some(a, a) }

    def fibs: Stream[Int] =
      unfold((0, 1)) { case (i1, i2) => Some(i1, (i2, i1 + i2)) }

    def from(n: Int): Stream[Int] =
      unfold(n) { n => Some(n, n + 1) }

    def map[A, B](as: Stream[A])(f: A => B): Stream[B] = unfold(as) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

    def ones: Stream[Int] =
      unfold(1)(Some(1, _))

    def take[A](as: Stream[A])(n: Int): Stream[A] = unfold((as, n)) {
      case (Cons(h, t), n_) if n_ > 0 => Some((h(), (t(), n_ - 1)))
      case _ => None
    }

    def takeWhile[A](as: Stream[A])(p: A => Boolean): Stream[A] = unfold(as) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

    def zipAll[A, B](as: Stream[A], bs: Stream[B]): Stream[(Option[A], Option[B])] = unfold((as, bs)) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
      case (Cons(ha, ta), Empty) => Some((Some(ha()), None), (ta(), empty))
      case (Empty, Cons(hb, tb)) => Some((None, Some(hb())), (empty, tb()))
      case _ => None
    }


    def zipWith[A, B, C](as: Stream[A], bs: Stream[B])(f: (A, B) => C): Stream[C] = unfold((as, bs)) {
      case (Cons(ha, ta), Cons(hb, tb)) => Some((f(ha(), hb()), (ta(), tb())))
      case _ => None
    }
  }

}