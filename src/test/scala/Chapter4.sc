import chapter4.{Option, Some}

Option.sequence(List(1, 3).map(Some(_).filter(_ > 0))).getOrElse(List())