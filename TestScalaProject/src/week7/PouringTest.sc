package week7

object PouringTest {
  val problem = new Pouring(Vector(4, 9))         //> problem  : week7.Pouring = week7.Pouring@2812cbfa
  problem.moves                                   //> res0: scala.collection.immutable.IndexedSeq[Product with Serializable with we
                                                  //| ek7.PouringTest.problem.Move] = Vector(Empty(0), Empty(1), Fill(0), Fill(1), 
                                                  //| Pour(0,1), Pour(1,0))
  problem.pathSets.take(3).toList                 //> res1: List[Set[week7.PouringTest.problem.Path]] = List(Set(-->Vector(0, 0)),
                                                  //|  Set(Fill(0)-->Vector(4, 0), Fill(1)-->Vector(0, 9)), Set(Fill(0) Fill(1)-->
                                                  //| Vector(4, 9), Fill(0) Pour(0,1)-->Vector(0, 4), Fill(1) Fill(0)-->Vector(4, 
                                                  //| 9), Fill(1) Pour(1,0)-->Vector(4, 5)))
  problem.solution(6)                             //> res2: Stream[week7.PouringTest.problem.Path] = Stream(Fill(1) Pour(1,0) Empt
                                                  //| y(0) Pour(1,0) Empty(0) Pour(1,0) Fill(1) Pour(1,0)-->Vector(4, 6), ?)
 
}