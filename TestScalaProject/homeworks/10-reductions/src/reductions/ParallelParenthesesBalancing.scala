package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /**
   * Returns `true` iff the parentheses in the input `chars` are balanced.
   */

  def balance(chars: Array[Char]): Boolean = {
    val length = chars.length
    @tailrec
    def loopBalance(i: Int, count: Int): Boolean = {
      if (i == length) count == 0
      else if (chars(i) == ')' && count == 0) false
      else if (chars(i) == '(') loopBalance(i + 1, count + 1)
      else if (chars(i) == ')') loopBalance(i + 1, count - 1)
      else loopBalance(i + 1, count)
    }
    loopBalance(0, 0)
  }

  /*
  //OutOfMemory: Java Heap Space
  def balance(chars: Array[Char]): Boolean = {
    def loopBalance(chars: Array[Char], count: Int): Boolean = {
      if (count < 0) false
      else if (chars.isEmpty) count == 0
      else if (chars.head == ')') loopBalance(chars.tail, count - 1)
      else if (chars.head == '(') loopBalance(chars.tail, count + 1)
      else loopBalance(chars.tail, count)
    }

    loopBalance(chars, 0)
  }
*/
  /**
   * Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      @tailrec
      def loopTraverse(i: Int, accL: Int, accR: Int): (Int, Int) = {
        if (i == until) (accL, accR)
        else if (chars(i) == '(') loopTraverse(i + 1, accL + 1, accR)
        else if (chars(i) == ')')
          if (accL > 0) loopTraverse(i + 1, accL - 1, accR)
          else loopTraverse(i + 1, accL, accR + 1)
        else loopTraverse(i + 1, accL, accR)
      }

      loopTraverse(idx, 0, 0)
    }

    /*
    //OutOfMemory: Java Heap Space
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      def loopTraverse(chars: Array[Char], accL: Int, accR: Int): (Int, Int) = {
        if (chars.isEmpty) (accL, accR)
        else if (chars.head == '(') loopTraverse(chars.tail, accL + 1, accR)
        else if (chars.head == ')')
          if (accL > 0) loopTraverse(chars.tail, accL - 1, accR)
          else loopTraverse(chars.tail, accL, accR + 1)
        //if (accL <= 0) loopTraverse(chars.tail, accL, accR + 1)
        //else loopTraverse(chars.tail, accL - 1, accR)
        else loopTraverse(chars.tail, accL, accR)
      }

      loopTraverse(chars.slice(idx, until), arg1, arg2)
    }*/

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val (l, r) = parallel(reduce(from, mid), reduce(mid, until))
        val min = math.min(l._1, r._2)
        (l._1 + r._1 - min, l._2 + r._2 - min)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
