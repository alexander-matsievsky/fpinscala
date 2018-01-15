import chapter5.Stream
import org.scalacheck.Prop.{BooleanOperators, all, forAll}
import org.scalacheck.{Arbitrary, Gen, Properties}

object Chapter5Spec extends Properties("EXERCISE 5") {
  private val add = (_: Int) + (_: Int)
  private val negate = -(_: Int)
  private val positive = (_: Int) > 0
  private val fibs = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181,
    6765, 10946, 17711, 28657, 46368)

  property("1") = forAll { as: List[Int] =>
    Stream(as: _*).toList == as
  }

  property("2") = forAll { (as: List[Int], n: Int) =>
    val s = Stream(as: _*)
    all(
      "drop" |: s.drop(n).toList == as.drop(n),
      "take" |: s.take(n).toList == as.take(n)
    )
  }

  property("3") = forAll { as: List[Int] =>
    Stream(as: _*).takeWhile(positive).toList == as.takeWhile(positive)
  }

  property("4") = forAll { as: List[Int] =>
    Stream(as: _*).forAll(positive) == as.forall(positive)
  }

  property("5") = forAll { as: List[Int] =>
    Stream.wFoldRight.takeWhile(Stream(as: _*), positive).toList == as.takeWhile(positive)
  }

  property("6") = forAll { as: List[Int] =>
    val s = Stream(as: _*)
    Stream.wFoldRight.headOption(s) == s.headOption
  }

  property("7") = forAll { (as1: List[Int], as2: List[Int]) =>
    val s1 = Stream(as1: _*)
    val s2 = Stream(as2: _*)
    all(
      "append" |: s1.append(s2).toList == as1 ++ as2,
      "filter" |: s1.filter(positive).toList == as1.filter(positive),
      "flatMap" |: s2.flatMap(a => Stream(a, negate(a))).toList == as2.flatMap(a => List(a, negate(a))),
      "map" |: s1.map(negate).toList == as1.map(negate)
    )
  }

  property("8") = forAll(Gen.choose(0, 100), Arbitrary.arbitrary[Int]) { (x, y) =>
    Stream.constant(y).take(x).toList == List.fill(x)(y)
  }

  property("9") = forAll(Gen.choose(0, 100), Gen.choose(0, 10000)) { (x, y) =>
    Stream.from(y).take(x).toList == (y until y + x).toList
  }

  property("10") =
    Stream.fibs.take(25).toList == fibs

  property("11") = forAll(Gen.choose(0, 100), Gen.choose(0, 10000)) { (x, y) =>
    Stream.unfold(y) { n => Some((n, n + 1)) }.take(x).toList == Stream.from(y).take(x).toList
  }

  property("12") = forAll(Gen.choose(0, 100), Gen.choose(0, 10000)) { (x, y) =>
    all(
      "constant" |: Stream.wUnfold.constant(y).take(x).toList == Stream.constant(y).take(x).toList,
      "fibs" |: Stream.wUnfold.fibs.take(25).toList == fibs,
      "from" |: Stream.wUnfold.from(y).take(x).toList == Stream.from(y).take(x).toList,
      "ones" |: Stream.wUnfold.ones.take(x).toList == Stream.ones.take(x).toList
    )
  }

  property("13") = forAll { (as1: List[Int], as2: List[Int], n: Int) =>
    val s1 = Stream(as1: _*)
    val s2 = Stream(as2: _*)
    all(
      "map" |: Stream.wUnfold.map(s1)(negate).toList == as1.map(negate),
      "take" |: Stream.wUnfold.take(s1)(n).toList == as1.take(n),
      "takeWhile" |: Stream.wUnfold.takeWhile(s1)(positive).toList == as1.takeWhile(positive),
      "zipAll" |: Stream.wUnfold.zipAll(s1, s2).map { case (a, b) => a.getOrElse(0) + b.getOrElse(0) }.toList == {
        as1.zipAll(as2, 0, 0).map(add.tupled)
      },
      "zipWith" |: Stream.wUnfold.zipWith(s1, s2)(add).toList == as1.zip(as2).map(add.tupled)
    )
  }

  property("14") = forAll { (as1: List[Int], as2: List[Int]) =>
    Stream(as1: _*).startsWith(Stream(as2: _*)) == as1.startsWith(as2)
  }

  property("15") = forAll { as: List[Int] =>
    Stream(as: _*).tails.map(_.toList).toList == as.tails.toList
  }

  property("16") = forAll { as: List[Int] =>
    Stream(as: _*).scanRight(0)(add).toList == as.scanRight(0)(add)
  }
}
