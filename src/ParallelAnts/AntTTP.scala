package ParallelAnts

/*
object AntTTP {
  def main(args: Array[String]) {  
    val startTime = System.currentTimeMillis();

    val numAnts = 1
    val ants = new Array[AntActorTTP](numAnts)
    
    val isSerial = if ((args.length > 0) && (args(0).equals("serial"))) true else true
    val resultGatherer = if (isSerial) null else new ResultGatherer(startTime, ants.length)

    var i = 0
    
    // create Graph of heuristics
    val graph = new AntGraph
    
    val swaphomes = graph.addNode(SwapHomes, "Swap teams")
    val swaprounds = graph.addNode(SwapRounds, "Swap rounds")
    
    graph.connectAll
    
    /*
    val graph = new Graph[AntNode]
    
    val swapteams = new AntNode("Swap teams")
    val swaphomeaway = new AntNode("Swap home/away")
    
    graph.addNode(swapteams)
    graph.addNode(swaphomeaway)
    
    swapteams.neighbours ::= swaphomeaway
    swaphomeaway.neighbours ::= swapteams
    */
    
    val n = 4
    val dist = TTPProblem.generateRandomDistanceMatrix(n, 4.0)
    
    //val dist =  Array(Array[Double](0, 745, 665, 929), Array[Double](745, 0, 80, 337), Array[Double](665, 80, 0, 380), Array[Double](929, 337, 380, 0))
    
    val problem = new TTPProblem(dist, 8276.0 / 100)
    
    val sol = time(
  		problem.randomSolution,
  		(s: MatrixInt, t: Long) => {
  			printf("Spent %d ms generating a matrix with %d backtracks\n", t, problem.backtracks)
  			s
  		}
		)
    
    while (i < ants.length) {
      ants(i) = new AntActorTTP(problem, Utility.deepClone(sol), graph, graph.nodes.length, resultGatherer)
      i += 1
    }

    if (isSerial) {
      println("running serial")

      i = 0

      while (i < ants.length) {
        ants(i).act
        i += 1
      }

      val stopTime = System.currentTimeMillis();
      print("serial runtime in millis = ")
      println(stopTime - startTime)
    } 
    else {
      println("running parallel")
      resultGatherer.start

      i = 0

      while (i < ants.length) {
        ants(i).start
        i += 1
      }
      
      // resultGatherer will block on completion, and print times
    }
    
    println("Distances:")
    Utility.printMatrix(dist)
    
    println("Solution:")
    Utility.printMatrix(sol)
    
    val m = SwapRounds
    
    m.act(sol)
    println("Solution altered:")
    Utility.printMatrix(sol)
    
    println("Backtrack hotspots:")
    Utility.printMatrix(problem.positionFrequency)
    
    printf("isValid: %b\nViolations: %d\nFitness: %f\nDistance: %f\n", problem.isValid(sol), problem.numberOfViolations(sol), problem.fitness(sol), problem.totalDistance(sol))
  }
  
  def time[T, R](f: => T, res:(T, Long) => R) : R = {
		val startTime = System.currentTimeMillis
		res(f, System.currentTimeMillis - startTime)
  }
}
*/
