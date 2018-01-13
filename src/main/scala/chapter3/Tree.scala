package chapter3

import org.scalacheck.{Arbitrary, Gen}

sealed abstract class Tree[+A] {
  def depth: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => (l.depth + 1) max (r.depth + 1)
  }

  def fold[B](b: B)(f: (A, B) => B): B = this match {
    case Leaf(a) => f(a, b)
    case Branch(l, r) => l.fold(r.fold(b)(f))(f)
  }

  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(l.map(f), r.map(f))
  }

  def size: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size
  }

}

case class Leaf[+A](value: A) extends Tree[A]

case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  val gen: Gen[Tree[Int]] = Gen.oneOf(
    for {value <- Arbitrary.arbitrary[Int]} yield Leaf(value),
    for {left <- gen; right <- gen} yield Branch(left, right))

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(a) => a
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  object wFold {
    def depth[A](ta: Tree[A]): Int =
      ta.fold(0)((_, depth) => depth + 1)

    def map[A, B](ta: Tree[A])(f: A => B): Tree[B] = ???

    //      ta.fold(_: Tree[B])((a, tb) => Leaf(f(a)))

    def size[A](tb: Tree[A]): Int =
      tb.fold(0)((_, size) => size + 1)
  }

}