package fp.datastructures

sealed trait Tree[+A]

case class Leaf[A] (value: A) extends Tree[A]
case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  
  //25
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r) + 1
    }
  }
  
  //26
  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(n) => n
      case Branch(l, r) => maximum(l).max(maximum(r)) 
    }
  }
  
  //27
  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 0 
      case Branch(l, r) => 1 + depth(l).max(depth(r))
    }
  }
  
  //28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(n) => Leaf(f(n))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }
  
  //29
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(n) => f(n)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }
  
  def sizeViaFold[A](t: Tree[A]): Int = {
    //fold(t)(x => 1)((x, y) => 1 + x + y)
    fold(t)(x => 1)(1 + _ + _)
  }
  
  def maximumViaFold(t: Tree[Int]): Int = {
    //fold(t)(x => x)((x, y) => x.max(y))
    fold(t)(x => x)(_.max(_))
  }
  
  def depthViaFold[A](t: Tree[A]): Int = {
    fold(t)(x => 0)((x, y) => 1 + x + y)
  }
  
  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(a => Leaf(f(a)): Tree[B])((x, y) => Branch(x, y))
  }
  
  def main(args: Array[String]): Unit = {
    val tree = Branch(Branch(Leaf(12), Branch(Leaf(3), Leaf(4))), Leaf(8))
    println(" 0. tree: " + tree)
    println("25. size: " + Tree.size(tree))
    println("26. max: " + Tree.maximum(tree))
    println("27. depth: " + Tree.depth(tree))
    println("28. map: " + Tree.map(tree)(x => x * 2))
    println("29. sizeViaFold: " + Tree.sizeViaFold(tree))
    println("29. maximumViaFold: " + Tree.maximumViaFold(tree))
    println("29. depthViaFold: " + Tree.depthViaFold(tree))
    println("29. mapViaFold: " + Tree.mapViaFold(tree)(x => x * 2))
  }
}