package streams

/**
 * A main object that can be used to execute the Bloxorz solver
 */
object Bloxorz extends App {

  /**
   * A level constructed using the `InfiniteTerrain` trait which defines
   * the terrain to be valid at every position.
   */
  object InfiniteLevel extends Solver with InfiniteTerrain {
    val startPos = Pos(1, 3)
    val goal = Pos(5, 8)
  }

  //println("Infinite: " + InfiniteLevel.solution)
  
  /**
   * A simple level constructed using the StringParserTerrain
   */
  abstract class Level extends Solver with StringParserTerrain

  object LevelNoSolution extends Level {
    val level =
      """S-
        |oT
        |oo""".stripMargin
  }

  //println("LevelNoSolution: " + LevelNoSolution.solution)
  
  object LevelTwoSolutions extends Level {
    val level =
      """Sooo
        |o--o
        |o--o
        |oooT""".stripMargin
  }

  //println("LevelTwoSolutions: " + LevelTwoSolutions.solution)
  //println("LevelTwoSolutions: " + LevelTwoSolutions.allSolutions)


  object Level0 extends Level {
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
  }

  //println("Level0: " + Level0.solution)

  /**
   * Level 1 of the official Bloxorz game
   */
  object Level1 extends Level {
    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin
  }

  //println("Level1: " + Level1.solution)
  println("Level1: " + Level1.allSolutions)

  /**
   * Custom Level
   */
  object Level2 extends Level {
    val level =
      """ooooooooTo
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooooo
        |------ooo-""".stripMargin
  }

  //println("Level2: " + Level2.solution)

  /**
   * Custom Level
   */
  object Level3 extends Level {
    val level =
      """ooooooooTo
        |oooooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooooo
        |------ooS-""".stripMargin
  }

  //println("Level3: " + Level3.solution)
}
