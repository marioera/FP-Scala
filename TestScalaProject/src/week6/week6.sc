package week6

object week6 {
  val odds: Range = 1 to 101 by 2                 //> odds  : Range = Range 1 to 101 by 2
  val s: Range = 40 until 50                      //> s  : Range = Range 40 until 50
  
  odds forall (_ % 2 == 1)                        //> res0: Boolean = true
  
  val text = "Mario Rodriguez"                    //> text  : String = Mario Rodriguez
  
  val n = odds zip text                           //> n  : scala.collection.immutable.IndexedSeq[(Int, Char)] = Vector((1,M), (3,a
                                                  //| ), (5,r), (7,i), (9,o), (11, ), (13,R), (15,o), (17,d), (19,r), (21,i), (23,
                                                  //| g), (25,u), (27,e), (29,z))
  n unzip                                         //> res1: (scala.collection.immutable.IndexedSeq[Int], scala.collection.immutabl
                                                  //| e.IndexedSeq[Char]) = (Vector(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25,
                                                  //|  27, 29),Vector(M, a, r, i, o,  , R, o, d, r, i, g, u, e, z))
	
	text flatMap (List('.', _))               //> res2: String = .M.a.r.i.o. .R.o.d.r.i.g.u.e.z

	odds sum                                  //> res3: Int = 2601
	
	(1 to 5) flatMap (x => (6 to 10) map (y => (x, y)))
                                                  //> res4: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,6), (1,7
                                                  //| ), (1,8), (1,9), (1,10), (2,6), (2,7), (2,8), (2,9), (2,10), (3,6), (3,7), (
                                                  //| 3,8), (3,9), (3,10), (4,6), (4,7), (4,8), (4,9), (4,10), (5,6), (5,7), (5,8)
                                                  //| , (5,9), (5,10))
	((1 to 10) zip (10 to 20)) map (xy => xy._1 * xy._2) sum
                                                  //> res5: Int = 880

	def isPrime(n: Int): Boolean = {
		(2 until n) forall (d => n % d != 0)
	}                                         //> isPrime: (n: Int)Boolean
	
	(1 until 10) map (i => (1 until i) map (j => (i, j)))
                                                  //> res6: scala.collection.immutable.IndexedSeq[scala.collection.immutable.Index
                                                  //| edSeq[(Int, Int)]] = Vector(Vector(), Vector((2,1)), Vector((3,1), (3,2)), V
                                                  //| ector((4,1), (4,2), (4,3)), Vector((5,1), (5,2), (5,3), (5,4)), Vector((6,1)
                                                  //| , (6,2), (6,3), (6,4), (6,5)), Vector((7,1), (7,2), (7,3), (7,4), (7,5), (7,
                                                  //| 6)), Vector((8,1), (8,2), (8,3), (8,4), (8,5), (8,6), (8,7)), Vector((9,1), 
                                                  //| (9,2), (9,3), (9,4), (9,5), (9,6), (9,7), (9,8)))

}