package fp.errorhandling

sealed trait Option[+A]   {
  //1
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(g) => Some(f(g))
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
  
  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(g) => g
    }  
  }
  
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
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
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def main(args: Array[String]): Unit = {
    val option = Some(1)
    val optionNone = None
    println(" 0. option: " + option)
    println(" 0. optionNone: " + optionNone)
    println(" 1. map: " + option.map(x => x * 5))
    println(" 1. flatMap: " + option.flatMap(x => Some(x * 3)))
    println(" 1. getOrElse: " + option.getOrElse())
    println(" 1. getOrElse: " + optionNone.getOrElse(7))
    println(" 1. orElse: " + option.orElse(Some(10)))
    println(" 1. orElse: " + optionNone.orElse(Some(10)))
    println(" 1. filter: " + option.filter(x => x == 10))
    println(" 1. filter: " + option.filter(x => x != 10))
  }
}