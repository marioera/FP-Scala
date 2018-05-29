package week6

object nqueens {
  def queens(n: Int): Set[List[Int]] = {
  	def placeQueens(k: Int): Set[List[Int]] = {
  		if (k == 0) Set(List())
  		else
  			for {
  				queens <- placeQueens(k - 1)
  				col <- 0 until n
  				if isSafe(col, queens)
  			} yield col :: queens
  			}
  	placeQueens(n)
  }                                               //> queens: (n: Int)Set[List[Int]]
  
  def isSafe(col: Int, queens: List[Int]): Boolean = {
  	val row = queens.length
  	val queensWithRow = (row - 1 to 0 by -1) zip queens
  	queensWithRow forall {
  		case (r, c) => col != c && math.abs(col - c) != row - r
  	}
  }                                               //> isSafe: (col: Int, queens: List[Int])Boolean
  
  def show(queens: List[Int]) = {
  	val lines =
  		for (col <- queens.reverse)
  		yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  	
		"\n" + (lines mkString "\n")
  }                                               //> show: (queens: List[Int])String
  
	queens(4)                                 //> res0: Set[List[Int]] = Set(List(2, 0, 3, 1), List(1, 3, 0, 2))
  queens(5)                                       //> res1: Set[List[Int]] = Set(List(0, 3, 1, 4, 2), List(2, 0, 3, 1, 4), List(0,
                                                  //|  2, 4, 1, 3), List(2, 4, 1, 3, 0), List(1, 3, 0, 2, 4), List(3, 0, 2, 4, 1),
                                                  //|  List(4, 2, 0, 3, 1), List(4, 1, 3, 0, 2), List(3, 1, 4, 2, 0), List(1, 4, 2
                                                  //| , 0, 3))
  //(queens(4) map show) mkString "\n"

	//queens(8) take 3 map show
}