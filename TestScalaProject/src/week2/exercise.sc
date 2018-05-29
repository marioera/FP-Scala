package week2

import math.abs

object exercise {
	val tolerance = 0.0001;                   //> tolerance  : Double = 1.0E-4
	def isCloseEnough(x: Double, y: Double) =
		abs((x - y) / x) / x < tolerance  //> isCloseEnough: (x: Double, y: Double)Boolean
		
	def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
		def iterate(guess: Double): Double = {
			val next = f(guess)
			if (isCloseEnough(guess, next)) next
			else iterate(next)
		}
		iterate(firstGuess)
	}                                         //> fixedPoint: (f: Double => Double)(firstGuess: Double)Double
	
	fixedPoint(x => 1 + x/2)(1)               //> res0: Double = 1.999755859375
	
	def averageDamp(f: Double => Double)(x: Double): Double = {
		(x + f(x)) / 2
	}                                         //> averageDamp: (f: Double => Double)(x: Double)Double
	
	def sqrt(y: Double): Double = fixedPoint(averageDamp(x => (y / x)))(1)
                                                  //> sqrt: (y: Double)Double
	sqrt(2)                                   //> res1: Double = 1.4142135623746899
}