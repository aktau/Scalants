package ParallelAnts

import scala.annotation.tailrec
import scala.util.Random

import Utility._

abstract class Problem[S]

final class TTPProblem(val distances: MatrixDouble, val _penalty: Double) extends Problem[MatrixInt] {
	val random = new Random
	
	val numberOfTeams = distances.length
	val numberOfRounds = 2 * numberOfTeams - 2
	
	// diagnostic information is saved here
	var backtracks: Int = 0
	var positionFrequency: MatrixInt = Array.fill[Int](numberOfRounds, numberOfTeams)(0)
	
	var _optimalCost = 0.0
	def optimalCost = _optimalCost
	def optimalCost_=(optimum: Double) = _optimalCost = optimum
	def optimumKnown = if (optimalCost != 0.0) true else false
	
	def penalty = _penalty * _penaltyFactor
	var _penaltyFactor = 1.0
	def penaltyFactor = _penaltyFactor
	def penaltyFactor_=(factor: Double) = _penaltyFactor = factor
	
	def numberOfViolations(solution: MatrixInt) : Int = {
		var violations = 0
		
		val teamIndices = 0 until numberOfTeams
		val roundIndices = 0 until numberOfRounds
		
		// count consecutive matches constraint
		for (teamIndex <- teamIndices) {
			var consecutiveCounter = 0
			var teamIsAway = false
			
			for (round <- roundIndices) {
				val c = solution(round)(teamIndex)
				
				if (isAway(c) == teamIsAway) {
					consecutiveCounter += 1
				}
				else {
					consecutiveCounter = 1
					teamIsAway = !teamIsAway
				}
				
				if (consecutiveCounter > 3)
					violations += 1
			}
		}
		
		// count no repeat constraint violations
		for (teamIndex <- teamIndices) {			
			var lastTeam = 0
			
			for (round <- roundIndices) {
				val c = math.abs(solution(round)(teamIndex))
				
				if (c == lastTeam)
					violations += 1
					
				lastTeam = c
			}
		}
		
		violations
	}
	
	def totalDistance(solution: MatrixInt) : Double = {
		var distance = 0.0
		
		for (team <- 0 until numberOfTeams) {
			var playedAtLast = team
			var lastAway = false
			
			//D.infox("team = %d, round = X, numberOfTeams = %d, numberOfRounds = %d\n", team, numberOfTeams, numberOfRounds);
			for (round <- 0 until numberOfRounds) {
				val away = isAway(solution(round)(team))
				val location = if (away) toIndex(solution(round)(team)) else team
				val counts = away || lastAway
				
				//println("%d plays at %d, counts = %b (distances(%d, %d) = %f)" format (team + 1, location + 1, counts, playedAtLast + 1, location + 1, distances(playedAtLast)(location)))
				
				if (counts) {
					distance += distances(playedAtLast)(location) 
					
					playedAtLast = location
					lastAway = away
				}
			}
			
			if (lastAway) // come back home...
				distance += distances(playedAtLast)(team)
				
			//println("Team %d completed: distance for now = %f" format (team + 1, distance))
		}
				
		distance
	}
	
	def cost(solution: MatrixInt) : Double =
		totalDistance(solution) + numberOfViolations(solution) * penalty
	
	/**
	 * s = solution
	 * r = round
	 * t = team
	 * c = chosen
	 * 
	 * a = assignment (this one is only filled in by the backtrack algorithm
	 * br = backtrack removed (same explanation)
	 */
	final case class Backtrack(s: MatrixIntVector, r: Int, t: Int, c: Int, a: (Int, Int) = (-1, -1))
	
	def isValid(solution: MatrixInt) : Boolean = isValid(solution.map(_.map(Vector(_))))
	def isValid(solution: MatrixIntVector) : Boolean = {
		// first check for empty allowed lists
		
		//printMatrix(solution)
		
		if (solution.exists(_.exists(_.isEmpty)))
			return false
		
		//println("Yea got here!")
		
		// check that each round only has distinct numbers
		if (solution exists (row => { val k = row.filter(_.length == 1).map(e => math.abs(e(0))); k.distinct.length != k.length }))
			return false
		
		//println("Fucking a!")
		
		val allGameSeries = allColumns(solution)
		
		// check that each team plays another team maximum two times
		if (allGameSeries exists { games => !games
				.foldLeft(Array.fill(solution.length)(0))((acc, x) => if (x.length == 1) { acc(math.abs(x(0)) - 1) += 1; acc } else acc)
				.filter(_ > 2)
				.isEmpty
		}) return false
		
		//println("Getting finnicky!")
		
		// if a team already plays against 2 fixed opponents, it has to be one home and one away match
		if (allGameSeries exists { games => !games
				.foldLeft(Array.fill(solution.length)(0))((acc, x) => if (x.length == 1) { acc(math.abs(x(0)) - 1) += toIntSign(x(0)); acc } else acc)
				.filter(math.abs(_) == 2)
				.isEmpty
		}) return false
		
		// if a team plays another team, the other team has to play it of course
		
		// a team can't play the same team twice in a row
		// read: if there exists a gameseries in which a team plays against the same team consecutively...	
		if (allGameSeries exists { games => games
				.sliding(2)
				.filter(g => g(0).length == 1 && g(1).length == 1) // get rid of all games that still have choice
				.map(g => math.abs(g(0)(0)) == math.abs(g(1)(0)))
				.exists(_ == true)
		}) return false
		
		// check for more than three consecutive same-sign matches
		if (allGameSeries exists { games => math.abs(games.foldLeft(0)( (acc, x) => {
				if (math.abs(acc) > 3) 
					acc
				else {
					if (x.length == 1) {
						val t = toIntSign(x(0))
						if (toIntSign(acc) != t) t
						else acc + t
					}
					else 0
				}
			})) > 3
		}) return false
		
		true
	}
	
	/**
	 * These are just the hard constraints, soft constraints
	 * aren't checked
	 */
	def isHardValid(solution: MatrixInt) : Boolean = isHardValid(solution.map(_.map(Vector(_))))
	def isHardValid(solution: MatrixIntVector) : Boolean = {
		// check that each round only has distinct numbers
		if (solution exists (row => { val k = row.filter(_.length == 1).map(e => math.abs(e(0))); k.distinct.length != k.length }))
			return false
		
		val allGameSeries = allColumns(solution)
			
		// check that each team plays another team maximum two times
		if (allGameSeries exists { games => !games
				.foldLeft(Array.fill(solution.length)(0))((acc, x) => if (x.length == 1) { acc(math.abs(x(0)) - 1) += 1; acc } else acc)
				.filter(_ > 2)
				.isEmpty
		}) return false
		
		// if a team already plays against 2 fixed opponents, it has to be one home and one away match
		if (allGameSeries exists { games => !games
				.foldLeft(Array.fill(solution.length)(0))((acc, x) => if (x.length == 1) { acc(math.abs(x(0)) - 1) += toIntSign(x(0)); acc } else acc)
				.filter(math.abs(_) == 2)
				.isEmpty
		}) return false
		
		true
	}
	
	def randomSolution : MatrixInt = {
		// TODO: if after backtracking there is only one option left (== assignment), check if it is valid
		// if not: reinitialize
		// +TODO: improve pickposition to pick the one with least domain left (== standard technique)
		// TODO: revise algorithm to branch less
		// TODO: inspect low-n case (N = 4) and check if more constraints are possible, feasible and help a lot
		// TODO: try to fix assign by default
		
		val numberOfRounds = 2*numberOfTeams - 2
		
		val teamIndices = 0 until numberOfTeams
		val roundIndices = 0 until numberOfRounds
		
		backtracks = 0
		positionFrequency = Array.fill[Int](numberOfRounds, numberOfTeams)(0)
		
		val constraints = new Constraints
		constraints.add(new TemporalConstraint(constraints, numberOfRounds, numberOfTeams))
		//constraints.add(new ReducedDomainSameRoundConstraint(constraints, numberOfRounds, numberOfTeams))
		constraints.add(new ConsecutiveConstraint(constraints, numberOfRounds, numberOfTeams))
		constraints.add(new OnePerGameSeriesConstraint(constraints, numberOfRounds, numberOfTeams))
		//constraints.add(new FastConsecutiveHomeAwayConstraint(constraints, numberOfRounds, numberOfTeams))
		constraints.add(new SlowConsecutiveHomeAwayConstraint(constraints, numberOfRounds, numberOfTeams))
		
		var backtrack: List[Backtrack] = List(Backtrack(
			Array.fill[Vector[Int]](numberOfRounds, numberOfTeams)(Vector.range(-numberOfTeams, numberOfTeams + 1) diff List(0)),
			0, 0, 0
		))
		
		//println("Fresh:")
		//printMatrix(backtrack.head.s)
		
		// trivial constraint: can't play against self
		for (teamIndex <- teamIndices; round <- roundIndices)
			backtrack.head.s(round)(teamIndex) = backtrack.head.s(round)(teamIndex).filter(math.abs(_) - 1 != teamIndex)
		
		//println("Cut:")
		//printMatrix(backtrack.head.s)
		
		var stop = false
		while (!stop) {
			val solution = deepClone(backtrack.head.s)
			
			val (round, team) = if (backtrack.head.a != (-1, -1)) {
				// this means that in the former loop, the backtracking algorithm made an assignment by removal of an offending choice (e.g.: (-2,8) -> (8))
				
				val position = backtrack.head.a
				
				//printf("BY DEFAULT ASSIGNMENT DETECTED (%d,%d)\n", position._1, position._2)
				
				backtrack = backtrack.head.copy(a = (-1,-1)) :: backtrack.tail
				position
			}
			else pickPosition(solution, teamIndices, roundIndices)
			
			//val (round, team) = pickPosition(solution, teamIndices, roundIndices)
			//val (round, team) = pickPositionSmallestDomain(solution, teamIndices, roundIndices)
			
			if (round == -1 || team == -1) {
				stop = true
			}
			else {
				// fill in diagnostic information
				positionFrequency(round)(team) += 1
				
				val chosen = pickFirstOpponent(solution(round)(team))
				val list = solution(round)(team)
				//val chosen = pickRandomOpponent(solution(round)(team))
				val oppositeIndex = toIndex(chosen)
				val oppositeFormer = solution(round)(oppositeIndex)
				val oppositeValue = invertedSign(chosen) * toTeam(team)
				
				//printf("Starting new loop (round = %d, team = %d, chosen = %d, backtracks = %d)\n", round, team, chosen, backtracks)
				
				if (!(solution(round)(toIndex(chosen)) contains oppositeValue)) {
					printMatrix(solution)
					printf("Starting new loop (round = %d, team = %d, chosen = %d, backtracks = %d)\n", round, team, chosen, backtracks)
					
					throw new Exception("With constraint processing on this shouldn't be possible")
				}
				
				backtrack ::= Backtrack(solution, round, team, chosen)
				
				backtrack.head.s(round)(team) = Vector(chosen)
				backtrack.head.s(round)(toIndex(chosen)) = Vector(oppositeValue)
				
				constraints.apply(backtrack.head.s, round, team, list, backtrack.head.s(round)(team))
				constraints.apply(backtrack.head.s, round, oppositeIndex, oppositeFormer, backtrack.head.s(round)(oppositeIndex)) 
				
				//printMatrix(solution)
				
				if (!isValid(backtrack.head.s)) {
					backtrack = recursiveBacktrack(backtrack)
					
					//println("Wasn't valid, backtracked...")
					//printMatrix(backtrack.head.s)
				}
			}
		}
		
		backtrack.head.s.map(_.map(_(0)))
	}
	
	@tailrec private def recursiveBacktrack(b: List[Backtrack]) : List[Backtrack] = {
		backtracks += 1
		
		val reduced = b.tail
		reduced.head.s(b.head.r)(b.head.t) = reduced.head.s(b.head.r)(b.head.t) diff Vector(b.head.c)
		
		if (!isValid(reduced.head.s))
			recursiveBacktrack(reduced)
		else if (reduced.head.s(b.head.r)(b.head.t).length == 1) // this is in fact an assignment, and we're going to signal it to the main algorithm
			reduced.head.copy(a = (b.head.r, b.head.t)) :: reduced.tail
		else
			reduced
	}
	
	private def pickPosition(solution: MatrixIntVector, teams: Range, rounds: Range) : (Int, Int) = {
		for (j <- teams; i <- rounds; if (solution(i)(j).length > 1))
			return (i, j)
		
		(-1, -1)
	}
	
	private def pickPositionSmallestDomain(solution: MatrixIntVector, teams: Range, rounds: Range) : (Int, Int) = {
		var min = 2 * numberOfTeams
		var chosen = (-1, -1)
		
		for (j <- teams; i <- rounds; if (solution(i)(j).length > 1 && solution(i)(j).length < min)) {
			chosen = (i, j)
			min =  solution(i)(j).length
		}
		
		chosen
	}
	
	private def pickFirstOpponent(list: Vector[Int]) : Int = list.head
	private def pickRandomOpponent(list: Vector[Int]) : Int = list(random.nextInt(list.length))
	
	//private def toIntSign(team: Int) : Int = if (team > 0) +1 else -1
	//private def isAway(team: Int) : Boolean = if (team > 0) false else true
	//private def toIndex(team: Int) : Int = math.abs(team) - 1
}

object TTPProblem {
	def generateRandomDistanceMatrix(teams : Int, upperBound : Double) : MatrixDouble = {
		val distances = new MatrixDouble(teams, teams)
		
		for (i <- 0 until teams ; j <- i + 1 until teams) {
			distances(i)(j) = math.random * upperBound
			distances(j)(i) = distances(i)(j)
		}

    distances
	}
	
	def getClassicProblem(teams: Int) : (MatrixDouble, Double) = teams match {
		case 4 => (Array(Array[Double](0, 745, 665, 929), Array[Double](745, 0, 80, 337), Array[Double](665, 80, 0, 380), Array[Double](929, 337, 380, 0)), 8276.0)
		case 6 => (Array(
			  Array[Double](0,	 745,	 665, 	929,  605, 521),  
			  Array[Double](745,   0,   80,  337, 1090,  315),  
			  Array[Double](665,  80,    0,  380, 1020,  257),  
			  Array[Double](929, 337,  380,    0, 1380,  408),  
			  Array[Double](605, 1090, 1020, 1380,   0, 1010),  
			  Array[Double](521,  315,  257,  408, 1010,   0)  
			), 22969)
		case 8 => (Array(
		      Array[Double](0, 745,  665,  929,  605,  521,  370,  587),
			  Array[Double](745,   0,   80,  337, 1090,  315,  567,  712),
			  Array[Double](665,  80,    0,  380, 1020,  257,  501,  664),
			  Array[Double](929, 337,  380,    0, 1380,  408,  622,  646),
			  Array[Double](605, 1090, 1020, 1380,   0, 1010,  957, 1190),
			  Array[Double](521,  315,  257,  408, 1010,   0,  253,  410),
			  Array[Double](370,  567,  501,  622,  957,  253,   0,  250),
			  Array[Double](587,  712,  664,  646, 1190,  410,  250,   0)
			), 39479)
		case 10 => (Array(
		      Array[Double](0,  745,  665,  929,  605,  521,  370,  587,  467,  670),
			  Array[Double](745,   0,   80,  337, 1090,  315,  567,  712,  871,  741),
			  Array[Double](665,  80,    0,  380, 1020,  257,  501,  664,  808,  697),
			  Array[Double](929, 337,  380,    0, 1380,  408,  622,  646,  878,  732),
			  Array[Double](605, 1090, 1020, 1380,   0, 1010,  957, 1190, 1060, 1270),
			  Array[Double](521,  315,  257,  408, 1010,   0,  253,  410,  557,  451),
			  Array[Double](370,  567,  501,  622,  957,  253,   0,  250,  311,  325),
			  Array[Double](587,  712,  664,  646, 1190,  410,  250,   0,  260,   86),
			  Array[Double](467,  871,  808,  878, 1060,  557,  311,  260,   0,  328),
			  Array[Double](670,  741,  697,  732, 1270,  451,  325,   86, 328,    0)
		    ), 59436)
		case _ => throw new Exception("Don't know that classic problem")
	}
}