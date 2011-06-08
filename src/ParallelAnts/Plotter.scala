package ParallelAnts

import org.jfree.data.xy.XYSeries
import org.jfree.data.xy.XYDataset
import org.jfree.data.xy.XYSeriesCollection
import org.jfree.chart.JFreeChart
import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.PlotOrientation
import javax.swing.JFrame
import org.jfree.chart.ChartPanel
import org.jfree.chart.plot.ValueMarker
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.renderer.xy.StandardXYItemRenderer
import java.awt.Color
import org.jfree.chart.renderer.xy.XYAreaRenderer
import org.jfree.chart.renderer.xy.XYAreaRenderer2

object Plotter {
  private var list: List[(Int, Double)] = Nil
  private var pureList: List[(Int, Double)] = Nil
  private var globalBestList: List[(Int, Double)] = Nil
  private var violations: List[(Int, Int)] = Nil
  private var markers: List[ValueMarker] = Nil
  
  private var lastId = 0
  private var iteration = 1
  
  private var _optimalValue = 0.0
  def optimalValue = _optimalValue
  def optimalValue_=(value: Double) = _optimalValue = value
  
  def updateIteration(id: Int) : Unit = {
    if (id != lastId) {
	    iteration += 1
	    
	    lastId = id
    }
  }
  
  def addViolations(id: Int, value: Int) : Unit = { 
    updateIteration(id)
    violations ::= (iteration, value) 
  }
  
  def addPureIterationResult(id: Int, value: Double) = {
    updateIteration(id)
    pureList ::= (iteration, value)
  }
  
  def addGlobalBestResult(id: Int, value: Double) = {
    updateIteration(id)
    globalBestList ::= (iteration, value)
  }
  
  def addIterationResult(id: Int, value: Double) = {
    updateIteration(id)
    list ::= (iteration, value)
  }
  
  def addMarker(id: Int, label: String = new String) = {
    updateIteration(id)
    
    markers ::= new ValueMarker(iteration)
    if (label.nonEmpty) markers.head.setLabel(label)
  }
  
  def addMarker(id: Int, color: Color) = {
    updateIteration(id)
    
    markers ::= new ValueMarker(iteration)
    markers.head.setPaint(color)
  }

  def plot = {
    val series = new XYSeries("Solution")
    val pureSeries = new XYSeries("Solution without penalty")
    val globalBestSeries = new XYSeries("Globally best solution")
    val optimalSeries = new XYSeries("Optimal solution")
    val violationSeries = new XYSeries("Violations")

    list.reverse.foreach(x => series.add(x._1, x._2))
    list.reverse.foreach(x => optimalSeries.add(x._1, optimalValue))
    globalBestList.reverse.foreach(x => globalBestSeries.add(x._1, x._2))
    pureList.reverse.foreach(x => pureSeries.add(x._1, x._2))
    violations.reverse.foreach(x => violationSeries.add(x._1,x._2))

    val xyDataset = new XYSeriesCollection()
    xyDataset.addSeries(series)
    xyDataset.addSeries(optimalSeries)
    xyDataset.addSeries(pureSeries)
    xyDataset.addSeries(globalBestSeries)
    
    val chart = ChartFactory.createXYLineChart(
      "Iterations vs. Cost", // Title
      "Iterations", // x-axis Label
      "Cost", // y-axis Label
      xyDataset, // Dataset
      PlotOrientation.VERTICAL, // Plot Orientation
      true, // Show Legend
      true, // Use tooltips
      false // Configure chart to generate URLs?
    )
    
    val plot = chart.getXYPlot
    
    plot.setBackgroundPaint(Color.white);
    
    // add markers
    markers.foreach((marker) => plot.addDomainMarker(marker))
    
    val domainAxis = plot.getDomainAxis();
    domainAxis.setRange(1, list.length);
    
    // add violations (needs new axis)
    val axis2 = new NumberAxis("Violations")
    axis2.setAutoRangeIncludesZero(false)
    plot.setRangeAxis(1, axis2)
    plot.setDataset(1, new XYSeriesCollection(violationSeries))
    plot.mapDatasetToRangeAxis(1, 1)
    
    val renderer2 = new StandardXYItemRenderer //new XYAreaRenderer2
    renderer2.setSeriesPaint(0, Color.cyan)
    //renderer2.setOutline(true)
    plot.setRenderer(1, renderer2)
    
    val frame = new JFrame("Scalants")
    
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setSize(640, 420)
    frame.add(new ChartPanel(chart))
    frame.pack()
    frame.setVisible(true)
  }
}