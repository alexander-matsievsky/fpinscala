import chapter3._
import org.scalacheck.Prop.{BooleanOperators, all, forAll}
import org.scalacheck.{Arbitrary, Gen, Properties}

import scala.collection.immutable.{List => StdList}

object Chapter3Spec extends Properties("EXERCISE 3") {
  val genTree: Gen[Tree[Int]] = Gen.oneOf(
    for {value <- Arbitrary.arbitrary[Int]} yield Leaf(value),
    for {left <- genTree; right <- genTree} yield Branch(left, right))
  private val add = (_: Int) + (_: Int)
  private val even = (_: Int) % 2 == 0

  property("1") = {
    3 == (List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    })
  }

  property("2") = forAll { (as: StdList[Int]) =>
    as.isEmpty ==> {
      List.tail(List(as: _*)).toStdList == StdList()
    } || as.nonEmpty ==> {
      List.tail(List(as: _*)).toStdList == as.tail
    }
  }

  property("3") = forAll { (as: StdList[Int], a: Int) =>
    List.setHead(List(as: _*), a).toStdList == a :: as.drop(1)
  }

  property("4") = forAll { (as: StdList[Int], i: Int) =>
    List.drop(List(as: _*), i).toStdList == as.drop(i)
  }

  property("5") = forAll { (as: StdList[Int]) =>
    List.dropWhile(List(as: _*), even).toStdList == as.dropWhile(even)
  }

  property("6") = forAll { (as: StdList[Int]) =>
    as.isEmpty ==> {
      List.init(List(as: _*)).toStdList == StdList()
    } || as.nonEmpty ==> {
      List.init(List(as: _*)).toStdList == as.init
    }
  }

  property("9") = forAll { (as: StdList[Int]) =>
    List.length(List(as: _*)) == as.length
  }

  property("10") = forAll { (as: StdList[Int]) =>
    List.foldLeft(List(as: _*), 0)(add) == as.sum
  }

  property("11") = forAll(Gen.containerOfN[StdList, Int](10, Gen.choose(0, 9))) { as =>
    val as2 = List(as: _*)
    val ds = List.foldRight(as2, Nil: List[Double])(Cons(_, _))
    all(
      "length" |: List.length(as2) == List.wFoldLeft.length(as2),
      "product" |: List.product2(ds) == List.wFoldLeft.product(ds),
      "sum" |: List.sum2(as2) == List.wFoldLeft.sum(as2)
    )
  }

  property("12") = forAll { (as: StdList[Int]) =>
    List.reverse(List(as: _*)).toStdList == as.reverse
  }

  property("13") = forAll { (as: StdList[Int]) =>
    val as2 = List(as: _*)
    all(
      "foldLeft" |: List.foldLeft(as2, "!")(_ ++ _.toString) == List.wFoldRight.foldLeft(as2, "!")(_ ++ _.toString),
      "foldRight" |: List.foldRight(as2, "!")(_.toString ++ _) == List.wFoldLeft.foldRight(as2, "!")(_.toString ++ _)
    )
  }

  property("14") = forAll { (as1: StdList[Int], as2: StdList[Int]) =>
    List.append(List(as1: _*), List(as2: _*)).toStdList == as1 ++ as2
  }

  property("15") = forAll { (las: StdList[StdList[Int]]) =>
    List.concat(List(las.map(List(_: _*)): _*)).toStdList == las.fold(StdList())(_ ++ _)
  }

  property("16") = forAll { (is: StdList[Int]) =>
    List.increment(List(is: _*)).toStdList == is.map(add.curried(1))
  }

  property("17") = forAll { (ds: StdList[Double]) =>
    List.doubleToString(List(ds: _*)).toStdList == ds.map(_.toString)
  }

  property("18") = forAll { (as: StdList[Int]) =>
    val increment = add.curried(1)
    List.map(List(as: _*))(increment).toStdList == as.map(increment)
  }

  property("19") = forAll { (as: StdList[Int]) =>
    List.filter(List(as: _*))(even).toStdList == as.filter(even)
  }

  property("20") = forAll { (as: StdList[Int]) =>
    List.flatMap(List(as: _*))(a => List(a, a)).toStdList == as.flatMap(a => StdList(a, a))
  }

  property("21") = forAll { (as: StdList[Int]) =>
    List.wFlatMap.filter(List(as: _*))(even).toStdList == as.filter(even)
  }

  property("22") = forAll { (is1: StdList[Int], is2: StdList[Int]) =>
    List.zipSum(List(is1: _*), List(is2: _*)).toStdList == is1.zip(is2).map(add.tupled)
  }

  property("23") = forAll { (is1: StdList[Int], is2: StdList[Int]) =>
    List.zipWith(List(is1: _*), List(is2: _*))(add).toStdList == is1.zip(is2).map(add.tupled)
  }

  property("24") = forAll { (sup: StdList[Int], sub: StdList[Int]) =>
    List.hasSubsequence(List(sup: _*), List(sub: _*)) == sup.containsSlice(sub)
  }
}
