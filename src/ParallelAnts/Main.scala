package ParallelAnts

import org.jfree.data.general.DefaultPieDataset
import org.jfree.chart.ChartFactory
import javax.swing.JFrame
import org.jfree.chart.ChartPanel

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

    val numAnts = 20
    val n = 8
    
    val (dist, optimalCost) = TTPProblem.getClassicProblem(n)
    val problem = new TTPProblem(dist, optimalCost / 40)
    problem.optimalCost = optimalCost
    
    // create Graph of heuristics
    //val graph = new NormalAntGraph
    val graph = new MinMaxAntGraph(1.0, 5.0)
    
    val p = problem
    object SuperSmartSwapHomes extends SwapHomes(p) with TTPHeuristic2DTeams with SuperSmartTTPHeuristic
    object SuperSmartSwapRounds extends SwapRounds(p) with TTPHeuristic2DRounds with SuperSmartTTPHeuristic
    object SuperSmartSwapTeams extends SwapTeams(p) with TTPHeuristic2DTeams with SuperSmartTTPHeuristic
    object SuperSmartShiftRounds extends ShiftRounds(p) with TTPHeuristic2DRounds with SuperSmartTTPHeuristic
    object SmartSwapHomes extends SwapHomes(p) with TTPHeuristic2DTeams with SmartTTPHeuristic
    object SmartSwapRounds extends SwapRounds(p) with TTPHeuristic2DRounds with SmartTTPHeuristic
    object SmartSwapTeams extends SwapTeams(p) with TTPHeuristic2DTeams with SmartTTPHeuristic
    object SmartShiftRounds extends ShiftRounds(p) with TTPHeuristic2DRounds with SmartTTPHeuristic
    
    // this is going to take a VERY long time
    object SuperSmartParialSwapRounds extends PartialSwapRounds(p) with TTPHeuristic2DRoundsRandomTeam with SuperSmartTTPHeuristic
    //object SuperSmartParialSwapRounds extends PartialSwapRounds(p) with TTPHeuristic2DRoundsRandomTeam with SuperSmartTTPHeuristic
    
    val supersmart = true
    
    val swaphomes = graph.addNode(new SwapHomes(p), "Swap homes")
    val swaprounds = graph.addNode(new SwapRounds(p), "Swap rounds")
    val swapteams = graph.addNode(new SwapTeams(p), "Swap teams")
    val partialswaprounds = graph.addNode(new PartialSwapRounds(p), "Partial swap rounds")
    val shiftrounds = graph.addNode(new ShiftRounds(p), "Shift rounds")
    if (supersmart) {
      val supersmartswaphomes = graph.addNode(SuperSmartSwapHomes, "Super Smart Swap homes")
      val supersmartswaprounds = graph.addNode(SuperSmartSwapRounds, "Super Smart Swap rounds")
      val supersmartswapteams = graph.addNode(SuperSmartSwapTeams, "Super Smart Swap teams")
      val supersmartshiftrounds = graph.addNode(SuperSmartShiftRounds, "Super Smart Shift rounds")
      val supersmartpartialswaprounds = graph.addNode(SuperSmartParialSwapRounds, "Super Smart Partial swap rounds")
    }
    else {
      val smartswaphomes = graph.addNode(SmartSwapHomes, "Smart Swap homes")
	  val smartswaprounds = graph.addNode(SmartSwapRounds, "Smart Swap rounds")
	  val smartswapteams = graph.addNode(SmartSwapTeams, "Smart Swap teams")
	  val smartshiftrounds = graph.addNode(SmartShiftRounds, "Smart Shift rounds")
    }

    graph.connectAll
    
	val controller = new AntController(numAnts, problem, graph, 0.9, 0.8, 10000) with LenientVisibility with MinMaxVisibility {
      val lowerVisibilityBound = 1.0
      val upperVisibilityBound = 5.0
    }
	/*
	val controller = new AntController(numAnts, problem, graph, 0.95, 0.7, 3000) with MinMaxVisibility { 
      val lowerVisibilityBound = 1.0
      val upperVisibilityBound = 5.0
    }
    */
    
    val run = Timef(
      controller.start,
      (_: Any, t: Long) => printf("Spent %d msec (%.3f sec) solving the problem\n", t, t.toDouble / 1000)
    )
    
    Plotter.optimalValue = optimalCost
    Plotter.plot
  }
}
