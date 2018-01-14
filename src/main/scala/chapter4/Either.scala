package chapter4

sealed trait Either[+E, +A] {
  def orElse[E2 >: E, A2 >: A](ea: => Either[E2, A2]): Either[E2, A2] =
    map { a => Right(a) } getOrElse ea

  def map[B](f: A => B): Either[E, B] =
    flatMap { a => Right(f(a)) }

  def flatMap[E2 >: E, B](f: A => Either[E2, B]): Either[E2, B] = this match {
    case Left(e) => Left(e: E2)
    case Right(a) => f(a)
  }

  def getOrElse[A2 >: A](a2: => A2): A2 = this match {
    case Left(_) => a2
    case Right(a) => a
  }

  def map2[E2 >: E, B, C](eb: Either[E2, B])(f: (A, B) => C): Either[E2, C] =
    for {a <- this; b <- eb} yield f(a, b)
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def sequence[E, A](eas: List[Either[E, A]]): Either[E, List[A]] =
    traverse(eas)(Right(_))

  def traverse[E, A, B](eas: List[Either[E, A]])(f: A => Either[E, B]): Either[E, List[B]] =
    eas.foldRight(Right(List()): Either[E, List[B]]) { (ea, ebs) =>
      for {a <- ea; b <- f(a); bs <- ebs} yield b :: bs
    }
}