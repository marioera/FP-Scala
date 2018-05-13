sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }
  }
  
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
  }
  
  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(l), z)((A, B) => f(B, A))
  }
  
  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRightViaFoldLeft(l, Nil:List[B])((h,t) => Cons(f(h), t))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil:List[A])((h, t) => if (f(h)) Cons(h, t) else t)
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    //concat(map(l)(f))
    l match {
      case Nil => Nil
      case Cons(h, t) => foldRightViaFoldLeft(f(h), flatMap(t)(f))((x, y) => Cons(x, y))
    }
  }
  
  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil:List[A])(append)
  }
  
  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }
  
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) List(a) else Nil)
  }
  
  def length[A](l: List[A]): Int = {
    l match {
      case Nil => 0
      case Cons(h, t) => 1 + length(t)
    }
  }

  def lengthFR[A](l: List[A]): Int = {
    foldRight(l, 0)((_, acc) => acc + 1)
  }

  def lengthFL[A](l: List[A]): Int = {
    foldLeft(l, 0)((acc, _) => acc + 1)
  }
  
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0)
      l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }
  }

  def setHead[A](l: List[A], n: A): List[A] = {
    l match {
      case Cons(_, t) => Cons(n, t)
    }
  }

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(List.reverse(a1), a2)((acc, h) => Cons(h, acc))
  }
  
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }
    go(l)
  }
  
  def addOne(l: List[Int]): List[Int] = {
    foldRight(l, Nil:List[Int])((h, t) => Cons(h + 1, t));
    //foldLeft(l, Nil:List[Int])((t, h) => Cons(h + 1, t));
  }

  def DoubleTOString(l: List[Double]): List[String] = {
    foldRight(l, Nil:List[String])((h, t) => Cons(h + "", t));
  }
  
  def addTwoIntLists(a1: List[Int], a2: List[Int]): List[Int] = {
    (a1, a2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addTwoIntLists(t1, t2))
    }
  }
  
  def addTwoLists[A, B, C](a1: List[A], a2: List[B])(f: (A, B) => C): List[C] = {
    (a1, a2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), addTwoLists(t1, t2)(f))
    }
  }
  
  def main(args: Array[String]): Unit = {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Cons(x, Cons(2, Cons(y, _))) => y
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(_, x) => x
      case Cons(1, Cons(2, Nil)) => 5
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    //print(x)

    val listNil = List()
    val list = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    val list2 = List(11, 12, 13, 14, 15, 16, 17, 18, 19, 100);
    val listDs = List(1.0, 2.1, 3.2, 4.3, 5.4, 6.5, 7.6, 8.7, 9.8, 10.9);
    println("List: " + list)
    println("Sum: " + List.sum(list))
    println("Tail: " + List.tail(list))
    println("Drop: " + List.drop(list, 3))
    println("DropWhile: " + List.dropWhile(list)(x => x <= 5))
    println("Append: " + List.append(list, list2))
    println("SetHead: " + List.setHead(list, 1000))
    println("Init: " + List.init(list))
    println("Init2: " + List.init(list))
    println("Init: " + List.init(listNil))
    
    println("FoldRight (Sum): " + List.foldRight(list, 0)((x, y) => x + y))
    println("FoldRight (Product): " + List.foldRight(list, 1)(_ * _))
    println("FoldRight (Nil Param): " + List.foldRight(list, Nil:List[Int])(Cons(_, _)))
    
    println("Length: " + List.length(list))
    println("LengthFR: " + List.lengthFR(list))

    println("FoldLeft (Sum): " + List.foldLeft(list, 0)((x, y) => x + y))
    println("FoldLeft (Product): " + List.foldLeft(list, 1)(_ * _))
    
    println("LengthFL: " + List.lengthFL(list))
    
    println("Reverse: " + List.reverse(list))
   
    println("Append Via Fold Right: " + List.appendViaFoldRight(list, list2));
    println("Append Via Fold Left: " + List.appendViaFoldLeft(list, list2));
    
    println("Add One: " + List.addOne(list));
    println("Double To String: " + List.DoubleTOString(listDs));
    
    println("Map: " + List.map(list)(x => x * 2));
    println("Filter: " + List.filter(list)(x => x % 2 == 0));
    println("FlatMap: " + List.flatMap(list)(x => List(x * 2, x * 2)));
    println("Filter Via FlatMap: " + List.filterViaFlatMap(list)(x => x % 2 == 0));
    
    println("Add Two Lists: " + addTwoIntLists(list, list2))
    println("Add Two Lists: " + addTwoLists(list, list2)((x, y) => x + y))
    println("Add Two Lists: " + addTwoLists(list, listDs)((x, y) => x + " " + y))
    
  }

}