package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  //println(printSet(x => x % 100 == 0))
  //println(printSet(filter(x => x % 100 == 0, x => x % 2 == 0)))
  //println(forall(x => x % 100 == 0, x => x % 2 == 0))
  //println(printSet(filter(x => x % 2 == 0, x => x % 2 != 0)))
}
