package ParallelAnts
import scala.collection.immutable.Queue
import scala.annotation.tailrec
import Utility._
import java.awt.Color

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
  
  private var globalBestSolution : MatrixInt = null
  
  private var stagnationCounter = 0
  private var improvementCounter = 0
  
  // the current iteration
  private var iteration = -1

  def start = {
    if (problem.numberOfTeams >= 10) {
      val sol = Timef(
  		problem.randomSolution,
  		(s: MatrixInt, t: Long) => {
  			printf("Spent %d ms generating a matrix with %d backtracks\n", t, problem.backtracks)
  			s
  		}
      )
      
      ants = (1 to numberOfAnts) map (id => 
        new Ant(problem, graph, sol, 2 * graph.nodes.length, id toString)
      ) toList
    }
    else {
      ants = (1 to numberOfAnts) map (id => 
        new Ant(problem, graph, problem.randomSolution, 2 * graph.nodes.length, id toString)
      ) toList
    }
    
    
    //ants = (1 to numberOfAnts) map (id => new Ant(problem, graph, problem.randomSolution, 3, id toString)) toList

    val (bestSolution, lowestCost) = copyBestSolution

    globalBestSolution = Utility.deepClone(bestSolution)
    
    val solution = improveSolution(bestSolution, lowestCost, iterations)

    println("Done! Cost: %f, Optimal cost: %f, number of violations: %d, cost without violations: %f" format (problem.cost(solution), problem.optimalCost, problem.numberOfViolations(solution), problem.totalDistance(solution)))
    printMatrix(solution)
    
    problem.penaltyFactor = 1.0
    println("Done! Optimal Cost: %f, Optimal cost: %f, number of violations: %d, cost without violations: %f" format (problem.cost(globalBestSolution), problem.optimalCost, problem.numberOfViolations(globalBestSolution), problem.totalDistance(globalBestSolution)))
    printMatrix(solution)
  }

  @tailrec private def improveSolution(bestSolution: MatrixInt, lowestCost: Double, iterations: Int): MatrixInt = {
    iteration = iterations
    
    if (problem.optimumKnown && lowestCost == problem.optimalCost) {
      println("OPTIMAL SOLUTION FOUND with %d iterations left" format (iterations))

      bestSolution
    } 
    else if (iterations != 0) {
      D.infox("Start of loop (%d left) --- (lowest cost = %.0f (without penalties = %.0f, penalty factor = %.3f, penalty = %.3f)\n", iterations, lowestCost, problem totalDistance bestSolution, problem.penaltyFactor, problem.penalty)
      D.infox("--------------------\n")
      D.infox(graph)
      
      // evaporate the pheromones on every path a little bit (a fitness attached to every jump from a heuristic to another heuristic)
      evaporatePheromones
      
      // lower the visibility of every heuristic a little bit (a cost attached to the heuristic)
      lowerVisibility
      
      // make the journeys
      ants foreach (ant => ant.makeJourney)
      
      // get the solution of the best and OR the former best solution if that was even better
      val (newBestSolution, newLowestCost) = retrieveBestSolution(bestSolution, lowestCost)
      
      // save the global best solution for later (this does nothing if the new solution is not the globally best one) 
      guardGlobalBest(newBestSolution)
      
      // strategic oscillation (adjust the penalty value for exploring both the feasible and infeasible ranges)
      strategicOscillation(bestSolution, lowestCost, newLowestCost)
      
      // write away some info about the new best solution that we can later use to plot
      writePlottingInfo(newBestSolution)
      
      // this is optional (we replace each ants solution by the current best)
      //ants foreach (ant => ant.solution = newBestSolution)
      
      //improveSolution(newBestSolution, newLowestCost, iterations - 1)
      improveSolution(newBestSolution, problem cost newBestSolution, iterations - 1)
    } 
    else {
      bestSolution
    }
  }

  protected def lowerVisibility =
    graph.nodes foreach (_.visibility *= visibilityDecay)

  @inline private def evaporatePheromones: Unit = {
    graph.nodes foreach { node =>
      node.neighbours foreach { neighbour =>
        node.changePheromone(neighbour, evaporateRate * node.pheromone(neighbour))
      }
    }
  }

  @inline private def copyBestSolution: (MatrixInt, Double) = {
    val lowestCost = ants map (_.cost) min
    
    val bestSolution = ants find (ant => ant.cost == lowestCost) map (_.solution) match {
      case Some(x) => deepClone(x)
      case None => throw new Exception("NO BEST SOLUTION FOUND")
    }

    (bestSolution, lowestCost)
  }
  
  @inline private def retrieveBestSolution(bestSolution: MatrixInt, lowestCost: Double) = {
    if (ants.map(ant => ant.cost).min <= lowestCost) {
      copyBestSolution
    } 
    else {
      (bestSolution, lowestCost)
    }
  }
  
  private val maxViolationLimit = 40
  private val percentagePerSearchPhase = 1.0 / (maxViolationLimit + 5) // add a few extra "periods" to stabilize the final solutions
  private val highPenaltyStagnationLimit = 75 //75
  private val lowPenaltyStagnationLimit = 40
  private val penaltyFactorAdjust = 0.90
  
  private var violationLimit = maxViolationLimit
  private var percentageDelta = 0.0
  private var lastIteration = -1

  @inline private def strategicOscillation(bestSolution: MatrixInt, lowestCost: Double, newLowestCost: Double) = {
    if (lastIteration == -1) lastIteration = iteration
    
    percentageDelta += math.abs(lastIteration - iteration).toDouble / iterations
    lastIteration = iteration
    if (percentageDelta > percentagePerSearchPhase) {
      violationLimit -= 1
      percentageDelta = 0.0
      
      Plotter.addMarker(iteration, Color.green)
    }
    
    if (newLowestCost == lowestCost) {
      stagnationCounter += 1

      if (stagnationCounter >= lowPenaltyStagnationLimit && problem.numberOfViolations(bestSolution) >= violationLimit) {
        //problem.penaltyFactor = 1
        if (problem.penaltyFactor < 1.0) {
          problem.penaltyFactor /= penaltyFactorAdjust

          Plotter.addMarker(iteration, Color.blue)
        }
        else if (problem.penaltyFactor < 10.0) {
          // this is the special red area where the penalties are placed really high to
          // absolutely force the solution to have no more violations, this will
          // usually load to quite suboptimal solutions
          
          problem.penaltyFactor /= penaltyFactorAdjust

          Plotter.addMarker(iteration, Color.pink)
        }
      }

      if (stagnationCounter >= highPenaltyStagnationLimit && problem.numberOfViolations(bestSolution) < violationLimit) {
        problem.penaltyFactor *= penaltyFactorAdjust

        Plotter.addMarker(iteration)
      }
    } 
    else {
      stagnationCounter = 0
    }
  }
  
  @inline private def guardGlobalBest(newBestSolution: MatrixInt) = {
    // temporarily reset the penalty factor to 1 so that we can make an honest comparison
    val oldPenaltyFactor = problem.penaltyFactor
    problem.penaltyFactor = 1
    
    val (newc, oldc) = (problem.cost(newBestSolution), problem.cost(globalBestSolution))
    
    if (newc < oldc) {
      Utility.copyValues(newBestSolution, globalBestSolution)
    }
    
    Plotter.addGlobalBestResult(iteration, math.min(oldc,newc))
    
    problem.penaltyFactor = oldPenaltyFactor
  }

  @inline private def writePlottingInfo(newBestSolution: MatrixInt) = {
    Plotter.addIterationResult(iteration, problem cost newBestSolution)
    Plotter.addPureIterationResult(iteration, problem.totalDistance(newBestSolution))
    Plotter.addViolations(iteration, problem.numberOfViolations(newBestSolution))
  }
}

/**
 * Helpful traits!
 */
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