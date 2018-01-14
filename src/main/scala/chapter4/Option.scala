package chapter4

sealed trait Option[+A] {
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def getOrElse[B >: A](b: => B): B = this match {
    case Some(a) => a
    case _ => b
  }

  def map[B](f: A => B): Option[B] =
    flatMap { a => Some(f(a)) }

  def map2[B, C](b: Option[B])(f: (A, B) => C): Option[C] =
    for {a <- this; b <- b} yield f(a, b)

  def filter(f: A => Boolean): Option[A] =
    flatMap { a => if (f(a)) Some(a) else None }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def mean(xs: Seq[Double]): Option[Double] =
    Some(xs).filter(_.nonEmpty).map { xs => xs.sum / xs.length }

  def traverse[A, B](oas: List[Option[A]])(f: A => Option[B]): Option[List[B]] =
    oas.foldRight(Some(List()): Option[List[B]]) { (oa, obs) =>
      for {a <- oa; b <- f(a); bs <- obs} yield b :: bs
    }

  def sequence[A](oas: List[Option[A]]): Option[List[A]] =
    oas.foldRight(Some(List()): Option[List[A]]) { (oa, oas) =>
      for {a <- oa; as <- oas} yield a :: as
    }
}