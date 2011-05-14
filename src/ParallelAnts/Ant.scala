package ParallelAnts

import scala.collection.immutable.Queue
import scala.annotation.tailrec
import scala.util.Random

import Utility._

final class Ant(val problem: TTPProblem, val graph: AntGraph, _solution: MatrixInt, val journeyLength: Int, name: String) {
  type Node = graph.Node
  
	type AntPath = Queue[Node]
  val Path = Queue
  
  val random = new Random
  
  val boost: Double = 5.0
  val lambda: Double = 1.0001
  //val lambda: Double = 1.002
  
  var currentNode: Node = graph.randomNode
  
  private var _cost = problem.cost(solution)
  private def cost_=(newcost: Double) = _cost = newcost
  def cost: Double = _cost
  
  def solution_=(newsolution: MatrixInt) = {
  	Utility.copyValues(newsolution, _solution)
  	
  	// refresh fitness value
  	cost = problem.cost(solution)
  }
  def solution: MatrixInt = _solution
  
  override def toString = name
  
  def makeJourney : Unit = {
  	val oldCost = cost
  	val path = findPath(Path(currentNode))
  	
  	printf("[%s] Path made: %s\n", this, path)
  	
  	currentNode = applyPath(solution, (path dequeue)._2)
  	
  	if (cost < oldCost)
  		updatePheromones(path, (oldCost - cost) / (path.length.toDouble - 1))
  }
  
  @tailrec
  private def findPath(currentPath: AntPath) : AntPath = {  	
  	if (currentPath.length % journeyLength == 0)
  		currentPath
		else
			findPath(currentPath enqueue chooseBestNeighbour(currentPath.last))
			//setupJourney(chooseBestNeighbour(currentPath.head) :: currentPath)
  }
  
  private def chooseBestNeighbour(node: Node) : Node = {
  	// selection probability calculation
  	val cumulativeWeights = node.neighbours
  		.map(neighbour => neighbour.visibility * (1.0 + node.pheromone(neighbour))) // convert nodes to weights
  		.scanLeft(0.0)(_+_).tail // sum weights
		
		val roulettePointer = random.nextFloat * cumulativeWeights.last
		
		printf("[%s] cumulative weights: %s\n[%s] roulette pointer: %f\n[%s] %s's neighbours %s\n",
			this, cumulativeWeights,
			this, roulettePointer,
			this, node.toShortString, node.neighbours)
		
		node.neighbours(cumulativeWeights.findIndexOf(_ > roulettePointer))
  }
  
  @tailrec private def updatePheromones(path: AntPath, increase: Double) : Unit = {
  	val (node, reduced) = path.dequeue
  	val pheromone = node.pheromone(reduced.head)
  	
  	node.changePheromone(reduced.head, pheromone + increase + boost)
  	
  	if (reduced.length > 1)
  		updatePheromones(reduced, increase)
  }
  
  private def applyPath(solution: MatrixInt, path: AntPath) : Node = {
  	var counter: Double = 1.0
  	
  	printf("[%s] Requested to apply path...\n", this)
  	
  	path.foldLeft(cost)(
			(lastcost, node) => {
				// make metaheuristic act
				val heuristicCost = time(node.heuristic.act(solution))
				cost = problem.cost(solution)
				
				if (!problem.isHardValid(solution)) {
					printMatrix(solution)
					
					throw new Exception("Solution not valid anymore")
				}
				
				// update visibility (heuristic) information
				printf("[cost %s from %f to %f] updating path (execution time of %s = %f): %f^%f / %f = %f (node: %s)\n",
					if (lastcost - cost > 0) "IMPROVED" else "WORSENED",
					lastcost,
					cost,
					node.heuristic,
					heuristicCost,
					lambda,
					lastcost - cost, 
					(heuristicCost * counter),
					math.pow(lambda, lastcost - cost) / (heuristicCost * counter),
					node
				)
				
				node.visibility += math.pow(lambda, lastcost - cost) / (heuristicCost * counter)
				
				counter += 1
				
				cost
			}
		)
  	
  	path.last
  }
  
  /**
   * Normally we would respond in milliseconds, but
   * the results were too small (which caused the visibility
   * to go up a lot...)
   */
  private def time[T](f: => Unit) : Double = {
		val startTime = System.nanoTime
		f
		(System.nanoTime - startTime) / 10000.0
  }
}