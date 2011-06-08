package ParallelAnts
import scala.util.Random

import Utility._

abstract class Heuristic[S] {
	def act(solution: S) : S
	
	val cost: Double
}

abstract class TTPHeuristic(val problem: TTPProblem) extends Heuristic[MatrixInt] {
	val random = new Random
	
	def act(solution: MatrixInt, move: Move) : MatrixInt
	
	@inline final def getRands(max: Int) : (Int, Int) = {
		val r1 = random.nextInt(max) 
		val r2 = {
			var k = random.nextInt(max)
			while (k == r1) k = random.nextInt(max)
			k
		}
		
		(r1,r2)
	}

  lazy val cost: Double = {
    val testMatrix = problem.randomSolution

    val iterations = 1000

    val start = System.currentTimeMillis
    for (i <- 1 to iterations) {
      act(testMatrix)
    }
    val end = System.currentTimeMillis
    
    val elapsed = if (end - start == 0) 1.0 else (end - start).toDouble
    
    printf("%scost of %s over %d iterations = %.3f sec (%.3f msec)\n", 
        if (end - start == 0) "[WAS NULL] " else "", toString, iterations, elapsed / 1000, elapsed)

    elapsed / 1000
  }
}

case class Move(r1: Int = 0, r2: Int = 0, t1: Int = 0, t2: Int = 0)
case class HyperMove(
    r1: Range = 0 to 0, 
    r2: Range = 0 to 0, 
    t1: Range = 0 to 0, 
    t2: Range = 0 to 0
)

trait TTPHeuristicDimensioned extends TTPHeuristic {
  def hypermove(solution: MatrixInt) : HyperMove
}
trait TTPHeuristic2DRounds extends TTPHeuristicDimensioned {
  override final def hypermove(solution: MatrixInt) = HyperMove(r1 = 0 until solution.length, r2 = 0 until solution.length)
}
trait TTPHeuristic2DTeams extends TTPHeuristicDimensioned {
  override final def hypermove(solution: MatrixInt) = HyperMove(t1 = 0 until solution(0).length, t2 = 0 until solution(0).length)
}
trait TTPHeuristic2DRounds1DTeam extends TTPHeuristicDimensioned {
  override final def hypermove(solution: MatrixInt) = HyperMove(r1 = 0 until solution.length, r2 = 0 until solution.length, t1 = 0 until solution(0).length)
}
trait TTPHeuristic2DRoundsRandomTeam extends TTPHeuristicDimensioned {
  override final def hypermove(solution: MatrixInt) = {
    val tstart = Random.nextInt(solution(0).length - 2)
    //val tend = tstart + 1 + Random.nextInt(solution(0).length - tstart)
    val tend = tstart + 2
    
    HyperMove(r1 = 0 until solution.length, r2 = 0 until solution.length, t1 = tstart until tend)
  }
}

trait SmartTTPHeuristic extends TTPHeuristic {
  self: TTPHeuristicDimensioned =>
  
    /*
  override def act(solution: MatrixInt): MatrixInt = {    
    val range = hypermove(solution)
    
    var bestSolution = solution
    var bestCost = problem cost solution

    for (
      r1 <- range.r1;
      r2 <- range.r2;
      t1 <- range.t1;
      t2 <- range.t2
    ) {
      // TODO: maybe try to close bestSolution and see how that fares, maybe good performance!
      //val copy = Utility.deepClone(solution)
      val copy = Utility.deepClone(bestSolution)
      act(copy, Move(r1, r2, t1, t2))

      val copyCost = problem cost copy
      if (copyCost < bestCost) {
        bestSolution = copy
        bestCost = copyCost
      }
    }
    
    Utility.copyValues(bestSolution, solution)
    
    solution
  }
  */
    
  override def act(solution: MatrixInt): MatrixInt = {    
    val range = hypermove(solution)
    
    val originalSolution = Utility.deepClone(solution)
    val bestSolution = Utility.deepClone(solution)
    
    var bestCost = problem.cost(solution)

    for (
      r1 <- range.r1;
      r2 <- range.r2;
      t1 <- range.t1;
      t2 <- range.t2
    ) {      
      act(solution, Move(r1, r2, t1, t2))
      
      val copyCost = problem.cost(solution)
      
      if (copyCost < bestCost) {
        Utility.copyValues(solution, bestSolution)
        bestCost = copyCost
      }
      else {
        // restore original solution only if it was worse
    	//Utility.copyValues(originalSolution, solution)
      }
      
      Utility.copyValues(originalSolution, solution)
    }
    
    Utility.copyValues(bestSolution, solution)
    
    solution
  }
  
  override def toString = "Smart " + super.toString
}

// keeps iterating until no more changes
trait SuperSmartTTPHeuristic extends SmartTTPHeuristic {
  self: TTPHeuristicDimensioned =>
    
  private var averageIterations = 0.0
  private var runs = 0 
  private var lastAveragingResults = scala.collection.immutable.Queue(0,0,0,0,0)
  
  private val movingAverageWidth = lastAveragingResults.length
  
  override def act(solution: MatrixInt): MatrixInt = {
    var cost = problem.cost(solution)

    var running = true
    var iterations = 0
    
    while (running) {
      super.act(solution)
      
      val newCost = problem.cost(solution)
      
      iterations += 1
      
      if (newCost == cost) {
        // stop if the solution stayed the same
        running = false
      }
      else if (newCost < cost) {
        cost = newCost
      }
      else {
        throw new Exception("This should NOT happen")
      }
    }
    
    val (pastIterations, clippedQueue) = lastAveragingResults.dequeue
    lastAveragingResults = clippedQueue.enqueue(iterations)
    
    // update running average
    averageIterations = averageIterations  + (iterations - pastIterations).toDouble / movingAverageWidth
    runs += 1
    
    solution
  }
  
  override def toString = ("Super (%.0f) " format (averageIterations)) + super.toString
}

class SwapRounds(problem: TTPProblem) extends TTPHeuristic(problem) {
  override def act(solution: MatrixInt): MatrixInt = {
    val (r1, r2) = getRands(solution.length)
    
    act(solution, Move(r1 = r1, r2 = r2))
  }
  
  @inline def act(solution: MatrixInt, move: Move) = {
    val temp = solution(move.r1)
    solution(move.r1) = solution(move.r2)
    solution(move.r2) = temp

    solution
  }

  override def toString = "Swap Rounds"
}

class OldShiftRounds(problem: TTPProblem) extends TTPHeuristic(problem) {
  override def act(solution: MatrixInt): MatrixInt = {
    val (r1, r2) = getRands(solution.length)
    
    act(solution, Move(r1 = r1, r2 = r2))
  }
  
  @inline def act(solution: MatrixInt, move: Move) = {
    var (min, max) = if (move.r1 < move.r2) (move.r1, move.r2) else (move.r2, move.r1)
    
    val temp = solution(min)

    while (min != max) {
      solution(min) = solution(min + 1)
      
      min += 1
    }
    
    solution(max) = temp

    solution
  }

  override def toString = "Shift Rounds"
}

class ShiftRounds(problem: TTPProblem) extends TTPHeuristic(problem) {
  override def act(solution: MatrixInt): MatrixInt = {
    val (r1, r2) = getRands(solution.length)
    
    act(solution, Move(r1 = r1, r2 = r2))
  }
  
  @inline def act(solution: MatrixInt, move: Move) = {
    var (min, max) = if (move.r1 < move.r2) (move.r1, move.r2) else (move.r2, move.r1)
    
    val temp = solution(min)

    while (min != max) {
      solution(min) = solution(min + 1)
      
      min += 1
    }
    
    solution(max) = temp

    solution
  }

  override def toString = "Shift Rounds"
}

class PartialSwapRounds(problem: TTPProblem) extends TTPHeuristic(problem) {
  private var partialPercentage = 0.0
  private var runs = 0

  override def act(solution: MatrixInt): MatrixInt = {
    val teams = solution(0).length
    val team = random.nextInt(teams)
    val (r1, r2) = getRands(solution.length)
    
    act(solution, Move(r1 = r1, r2 = r2, t1 = team))
  }
  
  @inline def act(solution: MatrixInt, move: Move) = {
    val teams = solution(0).length
    val (team, r1, r2) = (move.t1, move.r1, move.r2)

    // find all teams that are affected by this swap in rounds r1, r2
    // if this is ALL teams then this move becomes equivalent to a normal
    // swap rounds
    val seen = collection.mutable.Set(team, math.abs(solution(r1)(team)) - 1, math.abs(solution(r2)(team)) - 1)
    var toSee = List(math.abs(solution(r1)(team)) - 1, math.abs(solution(r2)(team)) - 1)

    while (toSee.nonEmpty) {
      //D.infox("seen:\t%s\ntoSee:\t%s\n", seen, toSee)

      val (t1, t2) = (math.abs(solution(r1)(toSee.head)) - 1, math.abs(solution(r2)(toSee.head)) - 1)

      toSee = toSee.tail

      if (!seen(t1)) {
        seen += t1
        toSee ::= t1
      }

      if (!seen(t2)) {
        seen += t2
        toSee ::= t2
      }
    }

    // update statistics (for debugging mostly)
    val success = if (seen.size == teams) 0 else 1
    partialPercentage = (runs * partialPercentage + success) / (runs + 1)

    runs += 1

    //if (seen.size == solution(0).length) D.infox("AWWW ALL FOR NOTHING: rounds %d and %d, teams %s\n", r1, r2, seen)
    //else D.infox("GOOD GOOD GOOD: rounds %d and %d [team %d]", r1, r2, team + 1)

    if (seen.size != teams) {
      for (sighted <- seen) {
        val temp = solution(r1)(sighted)
        solution(r1)(sighted) = solution(r2)(sighted)
        solution(r2)(sighted) = temp
      }
    } 
    else {
      // use faster codepath for complete swap
      val temp = solution(r1)
      solution(r1) = solution(r2)
      solution(r2) = temp
    }

    solution
  }

  override def toString = "Partial Swap Rounds (%.0f%%)" format (partialPercentage * 100)
}

class SwapHomes(problem: TTPProblem) extends TTPHeuristic(problem) {
  override def act(solution: MatrixInt): MatrixInt = {
    val (t1, t2) = getRands(solution(0).length)
    
    act(solution, Move(t1 = t1, t2 = t2))
  }

  @inline
  def act(solution: MatrixInt, move: Move) = {
    val rounds = solution.length
    val teams = solution(0).length

    val (t1, t2) = (move.t1, move.t2)

    for (c <- 0 until rounds) {
      if (math.abs(solution(c)(t1)) - 1 == t2) {
        solution(c)(t1) = -solution(c)(t1)
        solution(c)(t2) = -solution(c)(t2)
      }
    }

    solution
  }

  override def toString = "Swap Homes"
}

class SwapTeams(problem: TTPProblem) extends TTPHeuristic(problem) {
  override def act(solution: MatrixInt): MatrixInt = {
    val (t1, t2) = getRands(solution(0).length)

    act(solution, Move(t1 = t1, t2 = t2))
  }

  @inline
  def act(solution: MatrixInt, move: Move) = {
    val rounds = solution.length
    val teams = solution(0).length

    val (t1, t2) = (move.t1, move.t2)

    //printMatrix(solution)
    //println("SwapTeams :: swapping teams T%d and T%d" format (t1 + 1, t2 + 1))

    for (c <- 0 until rounds) {
      if (math.abs(solution(c)(t1)) - 1 != t2) {
        val s1 = toIndex(solution(c)(t1))
        val s2 = toIndex(solution(c)(t2))

        val temp = solution(c)(t1)
        solution(c)(t1) = solution(c)(t2)
        solution(c)(t2) = temp

        solution(c)(s1) = toIntSign(solution(c)(s1)) * (t2 + 1)
        solution(c)(s2) = toIntSign(solution(c)(s2)) * (t1 + 1)
      }
    }

    //printMatrix(solution)

    solution
  }

  override def toString = "Swap Teams"
}

object NullHeuristic extends TTPHeuristic(null) {
	override def act(solution: MatrixInt) = solution
	@inline override def act(solution: MatrixInt, move: Move) = solution
	override lazy val cost: Double = 0.0
	override def toString = "Null Heuristic"
}