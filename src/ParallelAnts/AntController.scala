package ParallelAnts
import scala.collection.immutable.Queue
import scala.annotation.tailrec
import Utility._

abstract class Controller {
  protected def lowerVisibility: Unit
}

class AntController(numberOfAnts: Int, problem: TTPProblem, val graph: AntGraph, evaporateRate: Double, val visibilityDecay: Double, iterations: Int = 100) extends Controller {
  type Node = graph.Node
  type AntPath = Queue[Node]
  val Path = Queue

  var ants: List[Ant] = Nil
  
  var pheromoneMin : Double = 1.0
  var pheromoneMax : Double = 5.0

  def start = {
    ants = (1 to numberOfAnts) map (id => new Ant(problem, graph, problem.randomSolution, graph.nodes.length, id toString)) toList
    //ants = (1 to numberOfAnts) map (id => new Ant(problem, graph, problem.randomSolution, 3, id toString)) toList

    val (bestSolution, lowestCost) = copyBestSolution

    val solution = improveSolution(bestSolution, lowestCost, iterations)

    println("Done! Cost: %f, Optimal cost: %f, number of violations: %d, cost without violations: %f" format (problem.cost(solution), problem.optimalCost, problem.numberOfViolations(solution), problem.totalDistance(solution)))
    printMatrix(solution)
  }

  @tailrec
  private def improveSolution(bestSolution: MatrixInt, lowestCost: Double, iterations: Int): MatrixInt = {
    if (problem.optimumKnown && lowestCost == problem.optimalCost) {
      println("OPTIMAL SOLUTION FOUND with %d iterations left" format (iterations))

      bestSolution
    } 
    else if (iterations != 0) {
      D.infox("Start of loop (%d left) --- (lowest cost = %.0f (without penalties = %.0f, penalty factor = %.3f)\n", iterations, lowestCost, problem totalDistance bestSolution, problem.penalty)
      D.infox("--------------------\n")
      D.infox(graph)
      
      evaporatePheromones
      lowerVisibility

      D.info("After evaporation\n--------------------")
      D.info(graph)
      
      // make the journeys
      D.infox("[%s 1] bestSolution = (%d,%d)\n", this, bestSolution.length, bestSolution(0).length)
      ants foreach (ant => {
        ant.makeJourney
      })
      D.infox("[%s 2] bestSolution = (%d,%d)\n", this, bestSolution.length, bestSolution(0).length)
      
      // find the minimal cost after the journeys have been made
      val posteriorLowestCost = ants map (_.cost) min

      val (newBestSolution, newLowestCost) =
        if (posteriorLowestCost <= lowestCost) {
          copyBestSolution
        } 
        else {
          (bestSolution, lowestCost)
        }

      // strategic oscillation
      
      /*
      if (problem.numberOfViolations(newBestSolution) > 0) 
        problem.penalty *= 1.001
      else 
        problem.penalty *= 0.999
      */
      
      D.info("After journey (%s <= %s)\n--------------------" format (posteriorLowestCost, lowestCost))
      D.info(graph)

      //printMatrix(newBestSolution)
      D.info("")
      
      D.infox("[%s 3] bestSolution = (%d,%d), newBestSolution = (%d,%d)\n", this, bestSolution.length, bestSolution(0).length, newBestSolution.length, newBestSolution(0).length)
      // this might be optional (we replace each ants solution by the current best)
      ants foreach (ant => ant.solution = newBestSolution)
      D.infox("[%s 4] bestSolution = (%d,%d), newBestSolution = (%d,%d)\n", this, bestSolution.length, bestSolution(0).length, newBestSolution.length, newBestSolution(0).length)

      improveSolution(newBestSolution, problem cost newBestSolution, iterations - 1)
    } 
    else {
      bestSolution
    }
  }

  protected def lowerVisibility: Unit =
    graph.nodes foreach (_.visibility *= visibilityDecay)

  private def evaporatePheromones: Unit = {
    graph.nodes foreach { node =>
      node.neighbours foreach { neighbour =>
        node.changePheromone(neighbour, evaporateRate * node.pheromone(neighbour))
      }
    }
  }

  private def copyBestSolution: (MatrixInt, Double) = {
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
    
  val lowerVisibilityBound: Double

  override protected def lowerVisibility: Unit =
    graph.nodes foreach (node => { node.visibility *= visibilityDecay; if (node.visibility < lowerVisibilityBound) node.visibility = lowerVisibilityBound })
}

trait LenientVisibility extends Controller {
  this: AntController =>
    
  val lowerVisibilityBound: Double
   
  override protected def lowerVisibility: Unit = {
    val visibilities = graph.nodes map (_.visibility)
    val avgVisibility = (visibilities sum) / graph.nodes.length
    val minVisibility = (visibilities min)
    val newVisibility = (avgVisibility + minVisibility) / 2

    //println("avgVisibility = %f, minVisibility = %f" format (avgVisibility, minVisibility))

    graph.nodes foreach (node => { node.visibility *= visibilityDecay; if (node.visibility < lowerVisibilityBound) node.visibility = newVisibility })
  }
}

trait MinMaxVisibility extends Controller {
  this: AntController =>
    
  val lowerVisibilityBound: Double
  val upperVisibilityBound: Double

  override protected def lowerVisibility: Unit =
    graph.nodes foreach (node => { 
      node.visibility *= visibilityDecay;
      node.visibility =
        if (node.visibility < lowerVisibilityBound)
          lowerVisibilityBound
        else if (node.visibility > upperVisibilityBound)
          upperVisibilityBound
        else
          node.visibility
    })
}