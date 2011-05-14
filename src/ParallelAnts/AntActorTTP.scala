package ParallelAnts

import scala.collection.immutable.Queue

import scala.annotation.tailrec

import scala.util.Random

import scala.actors.Actor
import scala.actors.Actor._

import ParallelAnts._

//objectiveFunction: List[Heuristic[Matrix]] => Int

/*
class AntActorTTP(val problem: TTPProblem, val solution: MatrixInt, val graph: AntGraph, val journeyLength: Int, val resultGatherer: ResultGatherer = null) extends Actor {
  type Node = graph.Node
  
	type AntPath = Queue[Node]
  val Path = Queue
  
  val random = new Random
  
  val boost: Double = 5.0
  val evaporateRate: Double = 0.5
  
  @tailrec
  private def journey(currentPath: AntPath) : AntPath = {  	
  	if (currentPath.length % journeyLength == 0)
  		currentPath
		else
			journey(currentPath enqueue chooseBestNeighbour(currentPath.last))
			//journey(chooseBestNeighbour(currentPath.last) :: currentPath)
  }
  
  def chooseBestNeighbour(node: Node) : Node = {
  	// selection probability calculation
  	val cumulativeWeights = node.neighbours
  		.map(neighbour => neighbour.visibility * (1.0 + neighbour.pheromone(node))) // convert nodes to weights
  		.scanLeft(0.0)(_+_).tail // sum weights
		
		val roulettePointer = random.nextFloat * cumulativeWeights.last
		
		println(node.neighbours)
		println(cumulativeWeights)
		
		node.neighbours(cumulativeWeights.findIndexOf(_ > roulettePointer))
  }
  
  private def evaporatePheromones : Unit = {
  	graph.nodes foreach {
  		node => node.neighbours foreach {
  			neighbour => node.changePheromone(neighbour, evaporateRate * node.pheromone(neighbour))
  		}
  	}
  }
  
  @tailrec private def updatePheromones(path: AntPath, increase: Double) : Unit = {
  	val (node, reduced) = path.dequeue
  	val pheromone = node.pheromone(reduced.head)
  	
  	node.changePheromone(node, pheromone + increase + boost)
  	
  	if (reduced.length > 1)
  		updatePheromones(reduced, increase)
  }
  
  private def applyPath(solution: MatrixInt, path: AntPath) : Node = {
  	path foreach (_.heuristic.act(solution))
  	
  	path.last
  }
  
  @tailrec private def improveSolution(startnode: Node, bestSolution: MatrixInt, bestFitness: Double, iterations: Int) {
  	if (iterations != 0) {
  		val path = journey(Path(startnode))
  		val newSolution = Utility.deepClone(bestSolution)
  		val newStartNode = applyPath(newSolution, path)
  		
  		val fitness = problem.fitness(newSolution)
  		
  		evaporatePheromones
  		
  		val (newBestSolution, newBestFitness) = 
  			if (fitness > bestFitness) {
  				updatePheromones(path, (fitness - bestFitness) / path.length.toDouble)
  				(newSolution, fitness)
  			}
				else 
					(solution, bestFitness)
  		
  		improveSolution(newStartNode, newBestSolution, newBestFitness, iterations - 1)
  	}
  }
  
	override def act() = {
		improveSolution(graph.randomNode, solution, problem.fitness(solution), 10000)
		
		if (resultGatherer != null) {
      resultGatherer ! Result(solution, problem.fitness(solution))
    } 
		else {
      Utility.printMatrix(solution)
      //print(" : ")
      //println(theBestPathLength)
    }
	}
}
*/