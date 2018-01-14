import chapter4.{Either, Option, Some}
import org.scalacheck.Prop.{BooleanOperators, all, forAll}
import org.scalacheck.Properties

object Chapter4Spec extends Properties("EXERCISE 4") {
  private val p = (_: Int) > 0

  property("1") = forAll { (xs: Seq[Double]) =>
    Option.mean(xs).getOrElse(0) == (if (xs.isEmpty) 0 else xs.sum / xs.length)
  }

  property("2") = forAll { (xs: Seq[Double]) =>
    Option.variance(xs).getOrElse(0) == (if (xs.isEmpty) 0 else {
      val m = xs.sum / xs.length
      xs.map(x => math.pow(x - m, 2)).sum / xs.length
    })
  }

  property("3") = forAll { (xs: Seq[Double]) =>
    val ys = Some(xs).filter(_.nonEmpty)
    ys.map2(ys)(_.sum / _.length).getOrElse(0) == (if (xs.isEmpty) 0 else xs.sum / xs.length)
  }

  property("4") = forAll { (xs: List[Int]) =>
    Option.sequence(xs.map(Some(_).filter(p))).getOrElse(List()) == (if (xs.forall(p)) xs else Nil)
  }

  property("5") = forAll { (xs: List[Int]) =>
    val positive = xs.map(Some(_).filter(p))
    Option.sequence(positive).getOrElse(List()) == Option.traverse(positive)(Some(_)).getOrElse(List())
  }

  property("7") = forAll { (xs: List[Int]) =>
    val positive = xs.map(chapter4.Right(_).flatMap { x =>
      if (p(x)) chapter4.Right(x) else chapter4.Left("negative integer")
    })
    all(
      "sequence" |: Either.sequence(positive).getOrElse(List()) == (if (xs.forall(p)) xs else Nil),
      "traverse" |: Either.sequence(positive).getOrElse(List()) ==
        Either.traverse(positive)(chapter4.Right(_)).getOrElse(List())
    )
  }
}