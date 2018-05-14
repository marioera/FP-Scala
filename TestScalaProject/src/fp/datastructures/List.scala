package fp.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]

object List {
  
  def sum(ints: List[Int]): Int = {
    ints match {
      case Nil => 0
      case Cons(h, t) => h + sum(t)
    }
  }
  
  def product(ints: List[Int]): Int = {
    ints match {
      case Nil => 1
      case Cons(h, t) => h * product(t)
    }
  }
  
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
  
  //2
  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, t) => t 
    }
  }
  
  //3
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0)
      l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }
  }
  
  //4
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t)(f) else Cons(h, t)
    }
  }

  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }
  }
  
  //5
  def setHead[A](l: List[A], n: A): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, t) => Cons(n, t)
    }   
  }
  
  //6
  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }
    
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }
  
  //7
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
  }
  
  def sum2(l: List[Int]): Int = foldRight(l, 0)(_ + _)
  def product2(l: List[Int]): Int = foldRight(l, 1)(_ * _)
  
  //8
  def theSame(l: List[Int]): List[Int] = {
    //foldRight(l, Nil:List[Int])(Cons(_, _))
    foldRight(l, Nil:List[Int])((h, t) => Cons(h, t))
  }

  //9
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, i) => i + 1)
  }
  
  //10
  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }
  
  //12
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil:List[A])((acc, h) => Cons(h, acc))
    //foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
  }
  
  //13
  def foldRightViafoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((acc, a) => f(a, acc))
  }
  
  //14
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((h, acc) => Cons(h, acc))
  }
  
  //15
  def concat[A](a1: List[List[A]]): List[A] = {
    foldRight(a1, Nil:List[A])((l1, acc) => append(l1, acc))
  }
  
  //16
  def add1(l: List[Int]): List[Int] = {
    foldRight(l, List[Int]())((h, acc) => Cons(h + 1, acc))
  }
  
  //17
  def toString(l: List[Double]): List[String] = {
    foldRight(l, Nil:List[String])((h, acc) => Cons("" + h, acc))
  }
  
  //18
  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil:List[B])((h, acc) => Cons(f(h), acc))
  }
  
  //19
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil:List[A])((h, acc) => if (f(h)) Cons(h, acc) else acc)
  }
  
  //20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    foldRight(l, Nil:List[B])((h, acc) => append(f(h), acc))
  }
  
  def flatMap2[A, B](l: List[A])(f: A => List[B]): List[B] = {
    concat(map(l)(f))
  }
  
  //21
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)((a) => if (f(a)) List(a) else Nil)
  }

  //23
  def addTwoLists[A](a1: List[A], a2: List[A])(f: (A, A) => A): List[A] = {
    (a1, a2) match {
      case (_, Nil) => a1
      case (Nil, _) => a2
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), addTwoLists(t1, t2)(f))
    }
  }
  
  //24
  def startsWith[A](l: List[A], sub: List[A]): Boolean = {
    (l, sub) match {
      case (_, Nil) => true
      case (Cons(h, t), Cons(subH, subT)) if (h == subH) => startsWith(t, subT)
      case _ => false
    }
  }
  
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    l match {
      case Nil => false
      case Cons(h, t) if (startsWith(l, sub)) => true
      case Cons(h, t) => hasSubsequence(t, sub) 
    }
  }
  
  def main(args: Array[String]): Unit = {
    val list = List(1, 2, 3, 4, 5)
    val list2 = List(6, 7, 8, 9, 10, 11)
    val list3 = List(list, list2)
    val list4 = List(1.1, 2.2, 3.3, 4.4, 5.5)
    val list5 = List(1, 2, 3)
    val list6 = List(2, 3, 4)
    println(" 1. List: " + list)
    println(" 2. tail: " + List.tail(list))
    println(" 3. drop: " + List.drop(list, 3))
    println(" 4. dropWhile: " + List.dropWhile(list)(x => x < 3))
    println(" 4. dropWhile2: " + List.dropWhile2(list)(x => x > 10))
    println(" 5. setHead: " + List.setHead(list, 3))
    println(" 6. append: " + List.append(list, list2))
    println(" 6. init: " + List.init(list))
    println(" 7. sum2: " + List.sum2(list))
    println(" 7. product2: " + List.product2(list))
    println(" 8. theSame: " + List.theSame(list))
    println(" 9. length: " + List.length(list))
    println("10. sum foldLeft: " + List.foldLeft(list, 0)(_ + _))
    println("10. product foldLeft: " + List.foldLeft(list, 1)(_ * _))
    println("10. length foldLeft: " + List.foldLeft(list, 0)((i, _) => i + 1))
    println("12. reverse: " + List.reverse(list))
    println("14. appendViaFoldRight: " + List.appendViaFoldRight(list, list2))
    println("15. concat: " + List.concat(list3))
    println("16. add1: " + List.add1(list))
    println("17. toString: " + List.toString(list4))
    println("19. filter: " + List.filter(list)(x => x % 2 == 0))
    println("20. flatMap: " + List.flatMap(list)(x => List(x * 2, x * 4)))
    println("20. flatMap2: " + List.flatMap2(list)(x => List(x * 2, x * 4)))
    println("21. filterViaFlatMap: " + List.filterViaFlatMap(list)(x => x % 2 == 0))
    println("23. addTwoLists: " + List.addTwoLists(list, list2)((x, y) => x + y))
    println("24. startsWith: " + List.startsWith(list, list5))
    println("24. hasSubsequence: " + List.hasSubsequence(list, list6))
    println("24. hasSubsequence: " + List.hasSubsequence(list, list2))
  }
}