package week7

trait Generator[+T] {

  self =>

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
		def generate = f(self.generate)
	}

	def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
		def generate = f(self.generate).generate
	}
}

object generators {

  val integers = new Generator[Int] {
    def generate = scala.util.Random.nextInt()
  }                                               //> integers  : week7.Generator[Int] = week7.generators$$anon$3@31cefde0

  val booleans = new Generator[Boolean] {
    def generate = integers.generate > 0
  }                                               //> booleans  : week7.Generator[Boolean] = week7.generators$$anon$4@47f37ef1

	val booleansMap = integers.map(x => x > 0)//> booleansMap  : week7.Generator[Boolean] = week7.Generator$$anon$1@5e025e70
  val booleansFor = for (x <- integers) yield x > 0
                                                  //> booleansFor  : week7.Generator[Boolean] = week7.Generator$$anon$1@45c8e616

  def pairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] = for {
    x <- t
    y <- u
  } yield (x, y)                                  //> pairs: [T, U](t: week7.Generator[T], u: week7.Generator[U])week7.Generator[(
                                                  //| T, U)]

	val pairsMap = integers.flatMap(x => integers.map(y => (x, y)))
                                                  //> pairsMap  : week7.Generator[(Int, Int)] = week7.Generator$$anon$2@7e0babb1
  val pairsFor = for {
  	x <- integers
  	y <- integers
  } yield (x, y)                                  //> pairsFor  : week7.Generator[(Int, Int)] = week7.Generator$$anon$2@3f0ee7cb

  pairsMap.generate                               //> res0: (Int, Int) = (369137600,-722927035)
	pairsFor.generate                         //> res1: (Int, Int) = (-1818617811,1071577102)
  booleans.generate                               //> res2: Boolean = false
  booleansMap.generate                            //> res3: Boolean = true
  booleansFor.generate                            //> res4: Boolean = false

	integers.map(x => integers.map(y => (x, y))).generate
                                                  //> res5: week7.Generator[(Int, Int)] = week7.Generator$$anon$1@180bc464
	integers.map(x => integers.map(y => (x, y))).generate.generate
                                                  //> res6: (Int, Int) = (-1579156644,1663888142)
                                                  
/* //Esto es lo que generaria: integers.map(x => integers.map(y => (x, y))).
	// Un Generator que al llamar al metodo generate devuelve un Generator. Por eso es que debe llamarse el .generate en el Flatmap
 new Generator[(Int, Int)] {
		def generate = 	new Generator[(Int, Int)] {
			def generate = (scala.util.Random.nextInt(), scala.util.Random.nextInt())
		}
	}

*/

	def single[T](x: T): Generator[T] = new Generator[T] {
		def generate = x
	}                                         //> single: [T](x: T)week7.Generator[T]
                                                  
 	def lists: Generator[List[Int]] = for {
 		isEmpty <- booleans
 		list <- if (isEmpty) emptyLists else nonEmptyLists
 	} yield list                              //> lists: => week7.Generator[List[Int]]
                                                  
	def emptyLists = single(Nil)              //> emptyLists: => week7.Generator[scala.collection.immutable.Nil.type]
	
	def nonEmptyLists = for {
		head <- integers
		tail <- lists
	} yield head :: tail                      //> nonEmptyLists: => week7.Generator[List[Int]]
		
	lists.generate                            //> res7: List[Int] = List()
	
	
	def leafs: Generator[Leaf] = for {
		x <- integers
	} yield Leaf(x)                           //> leafs: => week7.Generator[week7.Leaf]
	
	def inners: Generator[Inner] = for {
		l <- trees
		r <- trees
	} yield Inner(l, r)                       //> inners: => week7.Generator[week7.Inner]
	
	def trees: Generator[Tree] = for {
		isLeaf <- booleans
		tree <- if (isLeaf) leafs else inners
	} yield tree                              //> trees: => week7.Generator[week7.Tree]
 
 	trees.generate                            //> res8: week7.Tree = Inner(Leaf(1617496437),Inner(Leaf(-205965182),Leaf(96612
                                                  //| 4384)))
 	
 	def test[T](g: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit = {
 		for (i <- 0 until numTimes) {
 			val value = g.generate
 			assert(test(value), "test failed for " + value)
 		}
 		println("passed " + numTimes + " tests")
 	}                                         //> test: [T](g: week7.Generator[T], numTimes: Int)(test: T => Boolean)Unit
 	
 	test(pairs(lists, lists)) {
 		case (xs, ys) => (xs ++ ys).length >= xs.length
 	}                                         //> passed 100 tests
 	
}