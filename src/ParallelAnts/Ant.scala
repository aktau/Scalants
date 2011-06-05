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
  val lambda: Double = 1.003
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

  def makeJourney: Unit = {
    val oldCost = cost
    val path = findPath(Path(currentNode))

    D.info("[%s] Path made: %s\n", this, path map {_.toShortString} mkString (", "))
    
    // we take of the start node as we don't need to apply that anymore, and apply the rest
    currentNode = applyPath(solution, (path dequeue)._2)

    if (cost < oldCost)
      updatePheromones(path, (oldCost - cost) / (path.length.toDouble - 1))
  }

  @tailrec
  private def findPath(currentPath: AntPath): AntPath = {
    if (currentPath.length % journeyLength == 0)
      currentPath
    else
      findPath(currentPath enqueue chooseBestNeighbour(currentPath.last))
      //setupJourney(chooseBestNeighbour(currentPath.head) :: currentPath)
  }

  private def chooseBestNeighbour(node: Node): Node = {
    // selection probability calculation
    val cumulativeWeights = node.neighbours
      .map(neighbour => neighbour.visibility * (1.0 + node.pheromone(neighbour))) // convert nodes to weights
      .scanLeft(0.0)(_ + _).tail // sum weights

    val roulettePointer = random.nextFloat * cumulativeWeights.last

    D.info("[%s] cumulative weights: %s\n[%s] roulette pointer: %f\n",
      this, cumulativeWeights mkString (", "),
      this, roulettePointer
    )
    
    val index = cumulativeWeights.indexWhere(_ > roulettePointer)
    
    if (index == -1)
      D.infox("[%s] cumulative weights: %s\n[%s] roulette pointer: %f\n",
	    this, cumulativeWeights mkString (", "),
	    this, roulettePointer
	  )
    
    node.neighbours(index)
  }

  @tailrec
  private def updatePheromones(path: AntPath, increase: Double): Unit = {
    val (node, reduced) = path.dequeue
    val pheromone = node.pheromone(reduced.head)

    node.changePheromone(reduced.head, pheromone + increase + boost)

    if (reduced.length > 1)
      updatePheromones(reduced, increase)
  }

  private def applyPath(solution: MatrixInt, path: AntPath): Node = {
    var counter: Double = 1.0

    path.foldLeft(cost)(
      (lastcost, node) => {
        // make metaheuristic act
        node.heuristic.act(solution)
        val heuristicCost = node.heuristic.cost
        cost = problem.cost(solution)

        if (!problem.isHardValid(solution)) {
          printMatrix(solution)

          throw new Exception("Solution not valid anymore")
        }

        // update visibility (heuristic) information
        D.info("[%s]\t[cost %s from %.0f to %.0f] Visibility went from [%.2f] to [%.2f]\t%.6f^%.2f / %.2f = %.2f\n",
          node.heuristic,
          if (lastcost - cost > 0) "IMPROVED" else "WORSENED",
          lastcost,
          cost,
          node.visibility,
          node.visibility + math.pow(lambda, lastcost - cost) / (heuristicCost * counter),
          lambda,
          lastcost - cost,
          (heuristicCost * counter),
          math.pow(lambda, lastcost - cost) / (heuristicCost * counter)
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
   * 
   * DEPRECATED: this is no longer necessary, we precalculate the costs
   */
  /*
  private def time[T](f: => Unit): Double = {
    val startTime = System.nanoTime
    f
    (System.nanoTime - startTime) / 10000.0
  }
  */
}