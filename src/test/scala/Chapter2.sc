import chapter2.Chapter2._

(0 to 9) map ref.fib foreach println

val as = List(0, -1)
val ordered = (a1: Int, a2: Int) => a1 <= a2
as.zip(as.drop(1)).forall(ordered.tupled)
isSorted(as, ordered)

val add = (_: Int) + (_: Int)
val sub = (_: Int) - (_: Int)
compose(sub.curried(3), add.curried(3))(4)
