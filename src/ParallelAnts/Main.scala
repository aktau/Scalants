package ParallelAnts

abstract class DebugLevel
case object Warning extends DebugLevel
case object Error extends DebugLevel
abstract class Info extends DebugLevel
case object InnerLoopInfo extends Info
case object OuterLoopInfo extends Info
case object ImportantInfo extends Info

object Debug {
  def info(level: DebugLevel, text: String, xs: Any*) = Console.print(text.format(xs: _*))
  //def info(text: String, xs: Any*) : Unit = Console.print(text.format(xs: _*))
  //def infoln(x: Any) : Unit = Console.println(x)
  def infox(text: String, xs: Any*) : Unit = Console.print(text.format(xs: _*))
  def infox(x: Any) : Unit = Console.println(x)
  def info(text: String, xs: Any*) = ()
  def info(x: Any) = ()
}

object Main {
  def main(args : Array[String]) : Unit = {
  	val startTime = System.currentTimeMillis();

    val numAnts = 6
    val n = 8
    
    val (dist, optimalCost) = TTPProblem.getClassicProblem(n)
    val problem = new TTPProblem(dist, optimalCost / 50)
    problem.optimalCost = optimalCost
    
    // create Graph of heuristics
    //val graph = new NormalAntGraph
    val graph = new MinMaxAntGraph(1.0, 5.0)
    
    val p = problem
    object SmartSwapHomes extends SwapHomes(p) with TTPHeuristic2DTeams with SmartTTPHeuristic
    object SmartSwapRounds extends SwapRounds(p) with TTPHeuristic2DRounds with SmartTTPHeuristic
    object SmartSwapTeams extends SwapTeams(p) with TTPHeuristic2DTeams with SmartTTPHeuristic
    object SmartShiftRounds extends ShiftRounds(p) with TTPHeuristic2DRounds with SmartTTPHeuristic
    
    val swaphomes = graph.addNode(new SwapHomes(p), "Swap homes")
    val swaprounds = graph.addNode(new SwapRounds(p), "Swap rounds")
    val swapteams = graph.addNode(new SwapTeams(p), "Swap teams")
    val partialswaprounds = graph.addNode(new PartialSwapRounds(p), "Partial swap rounds")
    val shiftrounds = graph.addNode(new ShiftRounds(p), "Shift rounds")
    val smartswaphomes = graph.addNode(SmartSwapHomes, "Smart Swap homes")
    val smartswaprounds = graph.addNode(SmartSwapRounds, "Smart Swap rounds")
    val smartswapteams = graph.addNode(SmartSwapTeams, "Smart Swap teams")
    val smartshiftrounds = graph.addNode(SmartShiftRounds, "Smart Shift rounds")
    
    graph.connectAll
    
    //val dist = TTPProblem.generateRandomDistanceMatrix(n, 4.0)
    //val dist =  Array(Array[Double](0, 745, 665, 929), Array[Double](745, 0, 80, 337), Array[Double](665, 80, 0, 380), Array[Double](929, 337, 380, 0))
    
    val sol = time(
  		problem.randomSolution,
  		(s: MatrixInt, t: Long) => {
  			printf("Spent %d ms generating a matrix with %d backtracks\n", t, problem.backtracks)
  			s
  		}
	)
		
	//val controller = new AntController(numAnts, problem, graph, 0.9, 0.8, 1.0, 10000) with LenientVisibility
	val controller = new AntController(numAnts, problem, graph, 0.9, 0.8, 5000) with MinMaxVisibility { 
      val lowerVisibilityBound = 1.0
      val upperVisibilityBound = 5.0
    }
    
    val run = time(
      controller.start,
      (_: Any, t: Long) => printf("Spent %d msec (%.3f sec) solving the problem\n", t, t.toDouble / 1000)
    )
  }
  
  def time[T, R](f: => T, res:(T, Long) => R) : R = {
    val startTime = System.currentTimeMillis
    res(f, System.currentTimeMillis - startTime)
  }
}
