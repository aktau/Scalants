package ParallelAnts

object Main {
  def main(args : Array[String]) : Unit = {
  	val startTime = System.currentTimeMillis();

    val numAnts = 3
    val n = 6
    
    // create Graph of heuristics
    val graph = new AntGraph
    
    val swaphomes = graph.addNode(SwapHomes, "Swap homes")
    val swaprounds = graph.addNode(SwapRounds, "Swap rounds")
    val swapteams = graph.addNode(SwapTeams, "Swap teams")
    
    graph.connectAll
    
    //val dist = TTPProblem.generateRandomDistanceMatrix(n, 4.0)
    //val dist =  Array(Array[Double](0, 745, 665, 929), Array[Double](745, 0, 80, 337), Array[Double](665, 80, 0, 380), Array[Double](929, 337, 380, 0))
    
    val (dist, optimalCost) = TTPProblem.getClassicProblem(n)
    val problem = new TTPProblem(dist, optimalCost / 100)
    problem.optimalCost = optimalCost
    
    
    val sol = time(
  		problem.randomSolution,
  		(s: MatrixInt, t: Long) => {
  			printf("Spent %d ms generating a matrix with %d backtracks\n", t, problem.backtracks)
  			s
  		}
		)
		
		val controller = new AntController(numAnts, problem, graph, 0.9, 0.8, 1.0, 1000) with LenientVisibility
    controller.start
  }
  
  def time[T, R](f: => T, res:(T, Long) => R) : R = {
		val startTime = System.currentTimeMillis
		res(f, System.currentTimeMillis - startTime)
  }
}
