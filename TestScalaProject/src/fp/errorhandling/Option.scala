package fp.errorhandling

sealed trait Option[+A]   {
  //1
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(g) => Some(f(g))
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(g) => g
    }  
  }
  
  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }
  
  def flatMap1[B](f: A => Option[B]): Option[B] = {
    this match {
      case None => None
      case Some(g) => f(g)
    }  
  }
  
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this.map(Some(_)).getOrElse(ob)
  }
  
  def orElse1[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case None => ob
      case _ => this
      //case Some(g) => Some(g)
    }  
  }
  
  def filter(f: A => Boolean): Option[A] = {
    this match {
      case None => None
      case Some(g) => if (f(g)) Some(g) else None
    }  
  }
  
  def filter1(f: A => Boolean): Option[A] = {
    flatMap(a => if (f(a)) Some(a) else None)
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  
  //2
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }
  
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }
  
  //3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  }
  
  //5
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))   
    }
  }
  
  def sequence1[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))
  }
  
  def main(args: Array[String]): Unit = {
    val option = Some(1)
    val optionNone = None
    println(" 0. option: " + option)
    println(" 0. optionNone: " + optionNone)
    println(" 1. map: " + option.map(x => x * 5))
    println(" 1. flatMap: " + option.flatMap(x => if (x > 3) Some(x * 3) else None))
    println(" 1. getOrElse: " + option.getOrElse())
    println(" 1. getOrElse: " + optionNone.getOrElse(7))
    println(" 1. orElse: " + option.orElse(Some(10)))
    println(" 1. orElse: " + optionNone.orElse(Some(10)))
    println(" 1. filter: " + option.filter(x => x == 10))
    println(" 1. filter: " + option.filter(x => x != 10))
    
    val ds = List(1.1, 1.2, 2.1, 3.5, 4.6)
    val ds2 = List()
    println(" 2. mean: " + mean(ds))
    println(" 2. variance: " + variance(ds))
    println(" 2. mean: " + mean(ds2))
    println(" 2. variance: " + variance(ds2))
  }
}