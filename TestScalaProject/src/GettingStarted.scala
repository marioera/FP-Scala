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
  
  def main(args: Array[String]) : Unit = {
    println(formatResult("Abs", -42, abs));
    println(formatResult("Factorial", 3, factorial));
    println(formatResult("Fibonnaci", 10, fib));
    println(formatResult("Increment", 7, x => x + 1));
    println(formatResult("Double", 19, x => x * 2));
  }
  
}