package week6

object pairs {
	def isPrime(n: Int): Boolean = {
		(2 until n) forall (d => n % d != 0)
	}                                         //> isPrime: (n: Int)Boolean

  (1 until 10) map (i => (1 until i) map (j => (i, j)))
                                                  //> res0: scala.collection.immutable.IndexedSeq[scala.collection.immutable.Index
                                                  //| edSeq[(Int, Int)]] = Vector(Vector(), Vector((2,1)), Vector((3,1), (3,2)), V
                                                  //| ector((4,1), (4,2), (4,3)), Vector((5,1), (5,2), (5,3), (5,4)), Vector((6,1)
                                                  //| , (6,2), (6,3), (6,4), (6,5)), Vector((7,1), (7,2), (7,3), (7,4), (7,5), (7,
                                                  //| 6)), Vector((8,1), (8,2), (8,3), (8,4), (8,5), (8,6), (8,7)), Vector((9,1), 
                                                  //| (9,2), (9,3), (9,4), (9,5), (9,6), (9,7), (9,8)))
  
  ((1 until 10) map (i => (1 until i) map (j => (i, j)))) flatten
                                                  //> res1: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,1
                                                  //| ), (3,2), (4,1), (4,2), (4,3), (5,1), (5,2), (5,3), (5,4), (6,1), (6,2), (6,
                                                  //| 3), (6,4), (6,5), (7,1), (7,2), (7,3), (7,4), (7,5), (7,6), (8,1), (8,2), (8
                                                  //| ,3), (8,4), (8,5), (8,6), (8,7), (9,1), (9,2), (9,3), (9,4), (9,5), (9,6), (
                                                  //| 9,7), (9,8))
  
  (1 until 10) flatMap (i => (1 until i) map (j => (i, j)))
                                                  //> res2: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,1
                                                  //| ), (3,2), (4,1), (4,2), (4,3), (5,1), (5,2), (5,3), (5,4), (6,1), (6,2), (6,
                                                  //| 3), (6,4), (6,5), (7,1), (7,2), (7,3), (7,4), (7,5), (7,6), (8,1), (8,2), (8
                                                  //| ,3), (8,4), (8,5), (8,6), (8,7), (9,1), (9,2), (9,3), (9,4), (9,5), (9,6), (
                                                  //| 9,7), (9,8))
  
  (1 until 10) flatMap (i => (1 until i) map (j => (i, j))) filter (pair => isPrime(pair._1 + pair._2))
                                                  //> res3: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2
                                                  //| ), (4,1), (4,3), (5,2), (6,1), (6,5), (7,4), (7,6), (8,3), (8,5), (9,2), (9,
                                                  //| 4), (9,8))
}