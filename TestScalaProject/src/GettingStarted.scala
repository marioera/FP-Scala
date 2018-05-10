object factorial {

  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }
  
  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1, n*acc);
    }
    
    go(n, 1);
  }
  
  def fib(n: Int): Int = {
    def go(n: Int, fib1: Int, fib2: Int): Int = {
      if (n <= 0) fib1
      else go (n-1, fib2, fib1 + fib2)
    }
    
    go(n, 0, 1);
  }
  
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d";
    msg.format(name, n, f(n));
  }
  
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    def go(i: Int): Boolean = {
      if (i == as.length - 1) true;
      else if (gt(as(i + 1), as(i))) go(i + 1);
      else false;
    }
    
    go(0);
  }
  
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    (b: B) => f(a, b)
  }
  
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }
  
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }
  
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
  
  def main(args: Array[String]): Unit = {
    println(formatResult("Abs", -42, abs));
    println(formatResult("Factorial", 3, factorial));
    println(formatResult("Fibonnaci", 10, fib));
    println(formatResult("Increment", 7, x => x + 1));
    println(formatResult("Double", 19, x => x * 2));
    
    var intArray = Array(1, 2, 3, 4);    
    println(isSorted(intArray, (x: Int, y: Int) => if (x > y) true else false));
    var doubleArray = Array(1.1, 2.2, 3.3, 4.4);    
    println(isSorted(doubleArray, (x: Double, y: Double) => if (x > y) true else false));

    var fPartial = partial1(30, (x: Int, y: Int) => if (x > y) true else false)
    println("Partial:" + fPartial(10))
    
    var fCurry = curry((x: Int, y: Int) => if (x > y) true else false);
    var fCurry2 = fCurry(30);
    println("Curry:" + fCurry2(10));
    println("Curry:" + fCurry(30)(10));
    
    var fUncurry = uncurry((x: Int) => (y: Int) => if (x > y) true else false);
    println("Uncurry:" + fUncurry(30, 10)); 
  }
  
}