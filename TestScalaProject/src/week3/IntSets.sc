package week3

object IntSets {
  val t1 = new NonEmpty(7, Empty, Empty)          //> t1  : week3.NonEmpty = {.7.}
  val t2 = new NonEmpty(9, Empty, Empty)          //> t2  : week3.NonEmpty = {.9.}
	val t3 = t2.incl(10)                      //> t3  : week3.IntSet = {.9{.10.}}
	val t4 = t1.union(t3)                     //> t4  : week3.IntSet = {{.7.}9{.10.}}
	val t5 = t4.incl(8)                       //> t5  : week3.IntSet = {{.7{.8.}}9{.10.}}
	val t6 = new NonEmpty(12, Empty, Empty)   //> t6  : week3.NonEmpty = {.12.}
	val t7 = t5.union(t6)                     //> t7  : week3.IntSet = {{{.7{.8.}}9{.10.}}12.}

	def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
                                                  //> singleton: [T](elem: T)week3.Cons[T]
	singleton(1)                              //> res0: week3.Cons[Int] = week3.Cons@256216b3
	singleton(true)                           //> res1: week3.Cons[Boolean] = week3.Cons@2a18f23c
	
	def nth[T](n: Int, xs: List[T]): T =
		if (n == 0) xs.head
		else nth(n - 1, xs.tail)          //> nth: [T](n: Int, xs: week3.List[T])T
		
	val list = new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Nil))))
                                                  //> list  : week3.Cons[Int] = week3.Cons@d7b1517
  nth(3, list)                                    //> res2: Int = 4
}

trait List[T] {
	def isEmpty: Boolean
	def head: T
	def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
	def isEmpty = false
}

class Nil[T] extends List[T] {
	def isEmpty = true
	def head:Nothing = throw new NoSuchElementException("Nil.head")
	def tail:Nothing = throw new NoSuchElementException("Nil.tail")
}

abstract class IntSet {
	def incl(x: Int): IntSet
	def contains(x: Int): Boolean
	def union(other: IntSet): IntSet
}

object Empty extends IntSet {
	def contains(x: Int): Boolean = false
	def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
	override def toString = "."
	def union(other: IntSet): IntSet = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
	def contains(x: Int): Boolean = {
		if (x < elem) left.contains(x)
		else if (x > elem) right.contains(x)
		else true
	}
	def incl(x: Int): IntSet = {
		if (x < elem) new NonEmpty(elem, left.incl(x), right)
		else if (x > elem) new NonEmpty(elem, left, right.incl(x))
		else this
	}
	override def toString = "{" + left + elem + right + "}"
	def union(other: IntSet): IntSet = {
		//((left.union(right)).union(other)).incl(elem)
		left.union(right.union(other.incl(elem)))
	}
}