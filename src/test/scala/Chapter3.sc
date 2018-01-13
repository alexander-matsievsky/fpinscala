import chapter3.{List, Tree}

val as = List(0, -1)
List.foldLeft(as, "!")(_ ++ _.toString)
List.wFoldRight.foldLeft(as, "!")(_ ++ _.toString)

List.foldRight(as, "!")(_.toString ++ _)
List.wFoldLeft.foldRight(as, "!")(_.toString ++ _)

scala.List(0).zip(scala.List(0)).map {
  case (a1, a2) => a1 + a2
}
List.zipSum(List(scala.List(0): _*), List(scala.List(0): _*)).toStdList

scala.List().containsSlice(scala.List())

//val sup = List(4, 1515641225, 2147483647, -377576267, -1, 0, 458005234, 1727727647, 656205042, 1311, -1698532153, -2147483648, -1)
//val sub = List(0, -1)
//List.hasSubsequence(sup, sub)
//List.hasSubsequence(List(), List())

val sup = scala.List()
val sub = scala.List()
List.hasSubsequence(List(sup: _*), List(sub: _*))
sup.containsSlice(sub)

Tree.gen