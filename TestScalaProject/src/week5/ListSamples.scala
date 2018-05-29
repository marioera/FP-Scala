package week5

trait List2[+A]

case object Nil2 extends List2[Nothing]
case class Cons[+A] (head: A, tail: List2[A]) extends List2[A]

object List2 {
  
  def apply[A](as: A*): List2[A] = {
    if (as.isEmpty) Nil2
    else Cons(as.head, apply(as.tail: _*))
  }
  
  def lenght[A](as: List2[A]): Int = {
    def loop(as: List2[A], acc: Int): Int = {
      as match {
        case Nil2 => acc
        case Cons(h, t) => loop(t, acc + 1) 
      }
    }
    
    loop(as, 0)
  }
  
  def last[A](as: List2[A]): A = {
    as match {
      case Nil2 => throw new Error("last of empty List")
      case Cons(h, Nil2) => h
      case Cons(h, t) => last(t)
    }
  }
  
  def init[A](as: List2[A]): List2[A] = {
    as match {
      case Nil2 => throw new Error("init of empty List")
      case Cons(h, Nil2) => Nil2
      case Cons(h, t) => Cons(h, init(t))
    }
  }
  
  def concat[A](as1: List2[A], as2: List2[A]): List2[A] = {
    as1 match {
      case Nil2 => as2
      case Cons(h, t) => Cons(h, concat(t, as2))
    } 
  }
  
  def flatten[A](as: List2[A]): List2[A] = {
    as match {
      case Nil2 => Nil2
      case Cons(h: List2[A], t) => concat(flatten(h), flatten(t))
      case Cons(h, t) => Cons(h, flatten(t))
    }
  }
  
  def main(args: Array[String]): Unit = {
    val l = List2(1, 2, 3, 4, 5, 6, 7, 8, 9, 100)
    val l2 = List2(11, 12, 13, 14, 15, 16, 17, 18, 19, 200)
    val l3 = List2(List2(1, 2), 3, 4, List2(5, List2(List2(6, List2(7), 8), 9)))
    
    println(l)
    println(lenght(l))
    println(last(l))
    println(init(l))
    println(concat(l, l2))
    println("s " + l3)
    println("sf " + flatten(l3))
  }
  
}