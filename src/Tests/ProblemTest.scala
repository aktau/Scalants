package Tests

import ParallelAnts._
import org.junit._
import Assert._

class ProblemTest {
	val dist =  Array(Array[Double](0, 745, 665, 929), Array[Double](745, 0, 80, 337), Array[Double](665, 80, 0, 380), Array[Double](929, 337, 380, 0))
	val problem = new TTPProblem(dist, 1000000000000.0)
	val solution = Array(
		Array(3,	4,	-1,	-2),
		Array(2,	-1,	4,	-3),
		Array(4,	-3,	2,	-1),
		Array(-3,	-4,	1,	2),
		Array(-2,	1,	-4,	3),
		Array(-4,	3,	-2,	1)
	)
	
	val invalidSolution = Array(
		Array(3,	4,	-1,	-2),
		Array(1,	-1,	4,	-3),
		Array(4,	-3,	2,	-1),
		Array(-3,	-4,	1,	2),
		Array(-2,	1,	-4,	3),
		Array(-4,	3,	-2,	1)
	)
	
	val invalidSolution2 = Array(
		Array(3,	4,	-1,	-2),
		Array(2,	-1,	4,	-3),
		Array(-4,	-3,	2,	1),
		Array(-3,	-4,	1,	2),
		Array(-2,	1,	-4,	3),
		Array(-4,	3,	-2,	1)
	)
	
  @Test def testScoreCorrect = 
  	assertEquals(8276.0, problem.cost(solution), 0.1)
	
	@Test def testValidSolution =
  	assertTrue(problem.isValid(solution))
  
  @Test def testInvalidSolution =
  	assertFalse(problem.isValid(invalidSolution))
  	
	@Test def testInvalidSolution2 =
  	assertFalse(problem.isValid(invalidSolution2))
}