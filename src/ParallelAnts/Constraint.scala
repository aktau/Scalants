package ParallelAnts

import Utility._

sealed class Constraints {
	private var constraints: List[Constraint] = Nil
	
	def add(constraint: Constraint) : Unit = constraints ::= constraint
	def apply(solution: MatrixIntVector, round: Int, team: Int, former: Seq[Int], current: Seq[Int]) : Unit = constraints foreach { _.apply(solution, round, team, former, current) }
}

sealed abstract class Constraint(constraints: Constraints, maxRounds: Int, maxTeams: Int) {
	val allValues = (-maxTeams to maxTeams) diff List(0)
	val allRows = (0 until maxRounds) toSeq
	
	val allPositives = (1 to maxTeams)
	val allNegatives = (-maxTeams to -1)
	
	def apply(solution: MatrixIntVector, round: Int, team: Int, former: Seq[Int], current: Seq[Int]) : Unit
	
	final protected def update(solution: MatrixIntVector, diff: Seq[Int], positions: Seq[(Int, Int)]) : Unit = positions foreach { update(solution, diff, _) }
	final protected def update(solution: MatrixIntVector, diff: Int, positions: Seq[(Int, Int)]) : Unit = update(solution, Seq(diff), positions)
	final protected def update(solution: MatrixIntVector, diff: Int, position: (Int, Int)) : Unit = update(solution, Seq(diff), position)
	final protected def update(solution: MatrixIntVector, diff: Seq[Int], position: (Int, Int)) : Unit = {
		val original = solution(position._1)(position._2)
		val reduced = original.diff(diff)
		
		if (reduced.length != original.length) {
			solution(position._1)(position._2) = reduced
			
			// re-run all constraints (propagate change)
			// (this is commented because it was found to not really help much)
			constraints.apply(solution, position._1, position._2, original, reduced)
		}
	}
	
	protected def invert(former: Seq[Int]) : Seq[Int] = allValues.diff(former)
	protected def invertRows(rows: Seq[Int]) : Seq[Int] = allRows diff rows
}

final class TemporalConstraint(constraints: Constraints, maxRounds: Int, maxTeams: Int) extends Constraint(constraints, maxRounds, maxTeams) {
	override def apply(solution: MatrixIntVector, round: Int, team: Int, former: Seq[Int], current: Seq[Int]) : Unit = {
		//require(former != current)
		
		invert(current) foreach {
			e => update(solution, invertedSign(e) * toTeam(team), (round, toIndex(e)))
		}
	}
}

/**
 * This is a different version of the TemporalConstraint, should update less positions and thus be more efficient
 */
final class ReducedDomainSameRoundConstraint(constraints: Constraints, maxRounds: Int, maxTeams: Int) extends Constraint(constraints, maxRounds, maxTeams) {
	override def apply(solution: MatrixIntVector, round: Int, team: Int, former: Seq[Int], current: Seq[Int]) : Unit = {
		//require(former != current)
		
		if (former == current) println("WOROAORAOAORARO")
		
		(former diff current) foreach {
			e => update(solution, invertedSign(e) * toTeam(team), (round, toIndex(e)))
		}
	}
}


final class ConsecutiveConstraint(constraints: Constraints, maxRounds: Int, maxTeams: Int) extends Constraint(constraints, maxRounds, maxTeams) {
	override def apply(solution: MatrixIntVector, round: Int, team: Int, former: Seq[Int], current: Seq[Int]) : Unit = {
		//require(former != current)
		
		if (current.length == 1) {
			val M = solution.length - 1 // note: has to be a capital letter (scala bug/feature)
			val positions = round match {
				case 0 => Seq((round+1, team))
				case M => Seq((round-1, team))
				case _ => Seq((round-1, team), (round+1, team))
			}
			
			update(solution, Seq(current(0), -current(0)), positions)
		}
	}
}

final class OnePerGameSeriesConstraint(constraints: Constraints, maxRounds: Int, maxTeams: Int) extends Constraint(constraints, maxRounds, maxTeams) {
	override def apply(solution: MatrixIntVector, round: Int, team: Int, former: Seq[Int], current: Seq[Int]) : Unit = {
		//require(former != current)
		
		if (current.length == 1) {
			val t = current(0)
			
			invertRows(Seq(round)) foreach { e => update(solution, t, (e, team)) }
		}
	}
}

final class FastConsecutiveHomeAwayConstraint(constraints: Constraints, maxRounds: Int, maxTeams: Int) extends Constraint(constraints, maxRounds, maxTeams) {
	override def apply(solution: MatrixIntVector, round: Int, team: Int, former: Seq[Int], current: Seq[Int]) : Unit = {
		//require(former != current)
		
		if (current.length == 1) {
			if (round >= 2 && round+1 < maxRounds) {
				// check the former 2 rounds plus this one
				val rounds = (round-2 to round)
				
				// check if all of them are assigned
				if (rounds forall { solution(_)(team).length == 1 }) {
					if (rounds forall { solution(_)(team)(0) < 0 }) // if all 3 of them negative...
						update(solution, allNegatives, (round+1, team))
					else if (rounds forall { solution(_)(team)(0) > 0 }) // if all 3 of them positive
						update(solution, allPositives, (round+1, team))
				}
			}
		}
	}
}

/**
 * This one also checks non-assigned matches! Could be a serious speed-up for some selection strategies cutting very
 * large parts of the search tree, trading in CPU needed per iteration though.
 */
final class SlowConsecutiveHomeAwayConstraint(constraints: Constraints, maxRounds: Int, maxTeams: Int) extends Constraint(constraints, maxRounds, maxTeams) {
	override def apply(solution: MatrixIntVector, round: Int, team: Int, former: Seq[Int], current: Seq[Int]) : Unit = {
		import scala.collection.mutable.Map
		
		//require(former != current)
		
		def allPositive(v: Vector[Int]) = v forall (_ > 0)
		def allNegative(v: Vector[Int]) = v forall (_ < 0)
		def cachedAllPositive(r: Int)(implicit m: Map[Int, Int]) = {
			val v = solution(r)(team)
			// the next line is a cool caching mechanism
			m.getOrElseUpdate(r, if (allPositive(v)) +1 else if (allNegative(v)) -1 else 0) > 0
		}
		def cachedAllNegative(r: Int)(implicit m: Map[Int, Int]) = {
			val v = solution(r)(team)
			// the next line is a cool caching mechanism (notice the two functions share caches
			m.getOrElseUpdate(r, if (allPositive(v)) +1 else if (allNegative(v)) -1 else 0) < 0
		}
		def updateIfInRange(difflist: Range, first: Int, last: Int) = {
			if (first >= 0 && solution(first)(team).length != 1) update(solution, difflist, (first, team))
			if (last <= maxRounds - 1 && solution(last)(team).length != 1) update(solution, difflist, (last, team))
		}
		
		val roundwindows = (math.max(round - 2, 0) to math.min(round + 2, maxRounds - 1)).sliding(3)
		implicit val signmap = scala.collection.mutable.Map[Int, Int]()
		
		roundwindows foreach { roundwindow =>
			if (roundwindow forall (cachedAllPositive(_))) updateIfInRange(allPositives, roundwindow.head - 1, roundwindow.last + 1)
			else if (roundwindow forall (cachedAllNegative(_))) updateIfInRange(allNegatives, roundwindow.head - 1, roundwindow.last + 1)
		}
	}
}