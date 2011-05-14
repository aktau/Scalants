package ParallelAnts
import scala.util.Random

import Utility._

abstract class Heuristic[S] {
	def act(solution: S) : S
}

abstract class TTPHeuristic extends Heuristic[MatrixInt] {
	val random = new Random
	
	@inline final def getRands(max: Int) : (Int, Int) = {
		val r1 = random.nextInt(max) 
		val r2 = {
			var k = random.nextInt(max)
			while (k == r1) k = random.nextInt(max)
			k
		}
		
		(r1,r2)
	}
}

object SwapRounds extends TTPHeuristic {
	override def act(solution: MatrixInt) : MatrixInt = {
		val (r1,r2) = getRands(solution.length)
		
		val temp = solution(r1)
		solution(r1) = solution(r2)
		solution(r2) = temp
		
		solution
	}
	
	override def toString = "Swap Rounds"
}

object SwapHomes extends TTPHeuristic {
	override def act(solution: MatrixInt) : MatrixInt = {
		val rounds = solution.length
		val teams = solution(0).length
		
		val (t1,t2) = getRands(teams)
		
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

object SwapTeams extends TTPHeuristic {
	override def act(solution: MatrixInt) : MatrixInt = {
		val rounds = solution.length
		val teams = solution(0).length
		
		val (t1,t2) = getRands(teams)
		
		//printMatrix(solution)
		println("SwapTeams :: swapping teams T%d and T%d" format (t1 + 1, t2 + 1))
		
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

object NullHeuristic extends TTPHeuristic {
	override def act(solution: MatrixInt) : MatrixInt = solution
	override def toString = "Null Heuristic"
}