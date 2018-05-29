package week5

import math.Ordering

object mergesort {
  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
		val n = xs.length / 2
		if (n == 0) xs
		else {
			def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
				case (Nil, ys) => ys
				case (xs, Nil) => xs
				case (x :: xs1, y :: ys1) =>
					if (ord.lt(x, y)) x :: merge(xs1, ys)
					else y :: merge(xs, ys1)
			}
			
			val (fst, snd) = xs.splitAt(n)
			merge(msort(fst), msort(snd))
		}
  }                                               //> msort: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[T]

	val nums = List(4, 8, -4, 3, 1 , 6)       //> nums  : List[Int] = List(4, 8, -4, 3, 1, 6)
	msort(nums)                               //> res0: List[Int] = List(-4, 1, 3, 4, 6, 8)
	
	val fruits = List("pi�a", "ciruela", "banano", "melocoton", "manzana")
                                                  //> fruits  : List[String] = List(piña, ciruela, banano, melocoton, manzana)
	msort(fruits)                             //> res1: List[String] = List(banano, ciruela, manzana, melocoton, piña)
	
	nums filter (x => x > 0)                  //> res2: List[Int] = List(4, 8, 3, 1, 6)
	nums filterNot (x => x > 0)               //> res3: List[Int] = List(-4)
	nums partition (x => x > 0)               //> res4: (List[Int], List[Int]) = (List(4, 8, 3, 1, 6),List(-4))
	
	nums takeWhile (x => x > 0)               //> res5: List[Int] = List(4, 8)
	nums dropWhile (x => x > 0)               //> res6: List[Int] = List(-4, 3, 1, 6)
	nums span (x => x > 0)                    //> res7: (List[Int], List[Int]) = (List(4, 8),List(-4, 3, 1, 6))
	
	def pack[T](xs: List[T]): List[List[T]] = xs match {
		case Nil => Nil
		case x :: xs1 =>
			val (first, rest) = xs span (y => y == x)
			first :: pack(rest)
	}                                         //> pack: [T](xs: List[T])List[List[T]]
	
	def encode[T](xs: List[T]): List[(T, Int)] = {
		def loop(xs: List[List[T]]): List[(T, Int)] = {
			xs match {
				case Nil => Nil
				case x :: xs1 => (x.head, x.length) :: loop(xs1)
			}
		}
		
		loop(pack(xs))
	}                                         //> encode: [T](xs: List[T])List[(T, Int)]
	
	def encodeMap[T](xs: List[T]): List[(T, Int)] = {
		pack(xs) map (ys => (ys.head, ys.length))
	}                                         //> encodeMap: [T](xs: List[T])List[(T, Int)]
	
	var letters = List("a", "a", "a", "b", "c", "c", "a")
                                                  //> letters  : List[String] = List(a, a, a, b, c, c, a)
	pack(letters)                             //> res8: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a)
                                                  //| )
  encodeMap(letters)                              //> res9: List[(String, Int)] = List((a,3), (b,1), (c,2), (a,1))
  
  nums.reduceLeft(_ + _)                          //> res10: Int = 18
	nums.foldLeft(1)(_ * _)                   //> res11: Int = -2304
	
	def concat[T](xs: List[T], ys: List[T]): List[T] = {
		xs.foldRight(ys)(_ :: _)
	}                                         //> concat: [T](xs: List[T], ys: List[T])List[T]
	
	concat(nums, letters)                     //> res12: List[Any] = List(4, 8, -4, 3, 1, 6, a, a, a, b, c, c, a)
	
	def mapFun[T, U](xs: List[T], f: T => U): List[U] = {
		(xs foldRight List[U]())((x, acc) => f(x) :: acc)
	}                                         //> mapFun: [T, U](xs: List[T], f: T => U)List[U]
	
	mapFun(nums, (x: Int) => x * x)           //> res13: List[Int] = List(16, 64, 16, 9, 1, 36)

	def lengthFun[T](xs: List[T]): Int = {
		(xs foldRight 0)((x, acc) => 1 + acc)
	}                                         //> lengthFun: [T](xs: List[T])Int
	
	lengthFun(nums)                           //> res14: Int = 6

}