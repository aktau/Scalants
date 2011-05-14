package ParallelAnts
import scala.collection.immutable.Queue
import scala.annotation.tailrec
import Utility._

abstract class Controller {
	protected def lowerVisibility : Unit
}

class AntController(numberOfAnts: Int, problem: TTPProblem, val graph: AntGraph, evaporateRate: Double, val visibilityDecay: Double, val lowerVisibilityBound: Double, iterations: Int = 100)
	extends Controller {
	type Node = graph.Node
	
	type AntPath = Queue[Node]
  val Path = Queue
	
	var ants: List[Ant] = Nil
	
	def start = {
		ants = (1 to numberOfAnts) map (id => new Ant(problem, graph, problem.randomSolution, graph.nodes.length, id toString)) toList
		
		
		val (bestSolution, lowestCost) = copyBestSolution
		
		val solution = improveSolution(bestSolution, lowestCost, iterations)
		
		println("Done! Cost: %f, Optimal cost: %f, number of violations: %d, cost without violations: %f" format (problem.cost(solution), problem.optimalCost, problem.numberOfViolations(solution), problem.totalDistance(solution)))
		printMatrix(solution)
	}
	
	@tailrec private def improveSolution(bestSolution: MatrixInt, lowestCost: Double, iterations: Int) : MatrixInt = {
			if (problem.optimumKnown && lowestCost == problem.optimalCost) {
				println("OPTIMAL SOLUTION FOUND with %d iterations left" format (iterations))
				
				bestSolution
			}
			else if (iterations != 0) {
  		printf("Start of loop (%d left) --- (lowest cost = %f == best solution cost = %f)\n--------------------\n", iterations, lowestCost, problem cost bestSolution)
  		println(graph)
  		
  		evaporatePheromones
  		lowerVisibility
  		
  		println("After evaporation\n--------------------")
  		println(graph)
  		
  		ants foreach (ant => {
  			ant.makeJourney
  			println(graph)
  		})
  		
  		val posteriorLowestCost = ants map (_.cost) min
  		
  		val (newBestSolution, newLowestCost) = 
  			if (posteriorLowestCost <= lowestCost) {
  				//println("NEW best solution accepted (cost = %f == %f)" format (posteriorLowestCost, problem cost (ants find (ant => ant.cost == posteriorLowestCost) map (_.solution) getOrElse bestSolution)))
  				//(ants find (ant => ant.cost == posteriorLowestCost) map (_.solution) getOrElse bestSolution, posteriorLowestCost)
  				copyBestSolution
  			}
				else {
					//println("FORMER best solution was kept (cost = %f == %f)" format (lowestCost, problem cost bestSolution))
					(bestSolution, lowestCost)
				}
  		
			println("After journey (%s <= %s)\n--------------------" format (posteriorLowestCost, lowestCost))
  		println(graph)
  		
  		//printMatrix(newBestSolution)
  		println("\n")
			
			// this might be optional (we replace each ants solution by the current best)
  		ants foreach (ant => ant.solution = newBestSolution)
					
  		improveSolution(newBestSolution, newLowestCost, iterations - 1)
  	}
  	else {
  		bestSolution
  	}
  }
	
	protected def lowerVisibility : Unit =
  	graph.nodes foreach (_.visibility *= visibilityDecay)
	
	private def evaporatePheromones : Unit = {
  	graph.nodes foreach {
  		node => node.neighbours foreach {
  			neighbour => node.changePheromone(neighbour, evaporateRate * node.pheromone(neighbour))
  		}
  	}
  }
	
	private def copyBestSolution : (MatrixInt, Double) = {
		val lowestCost = ants map (_.cost) min
		val bestSolution = ants find (ant => ant.cost == lowestCost) map (_.solution) match {
			case Some(x) => deepClone(x)
			case None => throw new Exception("NO BEST SOLUTION FOUND")
		}
		
		(bestSolution, lowestCost)
	}
}

trait LowerBoundVisibility extends Controller {
	this: AntController =>
	
	override protected def lowerVisibility : Unit =
  	graph.nodes foreach (node => { node.visibility *= visibilityDecay; if (node.visibility < lowerVisibilityBound) node.visibility = lowerVisibilityBound })
}

trait LenientVisibility extends Controller {
	this: AntController =>
	
	override protected def lowerVisibility : Unit = {
		val visibilities = graph.nodes map (_.visibility)
		val avgVisibility = (visibilities sum) / graph.nodes.length
		val minVisibility = (visibilities min)
		val newVisibility = (avgVisibility + minVisibility) / 2
		
		println("avgVisibility = %f, minVisibility = %f" format (avgVisibility, minVisibility))
		
		graph.nodes foreach (node => { node.visibility *= visibilityDecay; if (node.visibility < lowerVisibilityBound) node.visibility = newVisibility })
	} 	
}