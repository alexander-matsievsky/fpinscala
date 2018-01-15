import chapter5.Stream

val as1 = List(52, 7, -215, 0, -1, 30, -94269073, -28381921, -2147483648, -1800034016, 1552242708, 1113682645, -1, 2199879, 1)
val as2 = List(0)
Stream(as1: _*).startsWith(Stream(as2: _*)) == as1.startsWith(as2)
Stream(as1: _*).startsWith(Stream(as2: _*))
as1.startsWith(as2)

Stream(List(): _*).tails.map(_.toList).toList == List().tails.toList
Stream(List(): _*).tails.map(_.toList).toList
List().tails.toList

val add = (_: Int) + (_: Int)
val as3 = List(1, 2, 3)
Stream(as3: _*).scanRight(0)(add).toList == as3.scanRight(0)(add)
Stream(as3: _*).scanRight(0)(add).toList
as3.scanRight(0)(add)