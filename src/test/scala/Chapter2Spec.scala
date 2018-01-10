import chapter2.Chapter2._
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Properties

object Chapter2Spec extends Properties("EXERCISE 2") {
  private val add = (_: Int) + (_: Int)
  private val ordered = (a1: Int, a2: Int) => a1 <= a2
  private val sub = (_: Int) - (_: Int)

  property("1") = forAll { (i: Int) =>
    (i <= 0 && i < 100) ==> {
      ref.fib(i) == fib(i)
    }
  }

  property("2") = forAll { (as: List[Int]) =>
    isSorted(as, ordered) == as.zip(as.drop(1)).forall(ordered.tupled)
  }

  property("3") = forAll { (x: Int, y: Int) =>
    curry(add)(x)(y) == add(x, y)
  }

  property("4") = forAll { (x: Int, y: Int) =>
    uncurry(curry(add))(x, y) == add(x, y)
  }

  property("5") = forAll { (x: Int, y: Int) =>
    compose(sub.curried(y), add.curried(y))(x) == -x
  }

}
