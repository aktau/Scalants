package ParallelAnts

import scala.util.Random

abstract class Graph {
  type Node <: NodeIntf

  abstract class NodeIntf {
    def connectWith(node: Node): Unit
    def connectWithAll: Unit
    def neighbours: List[Node]
  }

  def nodes: List[Node]
  def addNode: Node

  def connectAll: Unit

  def randomNode: Node
}

abstract class UndirectedGraph extends Graph {
  type Node <: NodeImpl

  class NodeImpl extends NodeIntf {
    self: Node =>

    var neighbours: List[Node] = Nil

    def connectWith(node: Node): Unit = {
      if (!(neighbours contains node) && this != node)
        neighbours ::= node

      node.addReverseEdge(this)
    }

    def connectWithAll: Unit =
      nodes foreach { this connectWith _ }

    private[NodeImpl] def addReverseEdge(node: Node): Unit =
      if (!(neighbours contains node) && this != node) neighbours ::= node
  }

  var nodes: List[Node] = Nil

  def connectAll: Unit =
    nodes foreach { _.connectWithAll }

  def randomNode: Node =
    nodes(Random.nextInt(nodes.length))
}

abstract class AntGraph extends UndirectedGraph {
  type Node <: AntNode

  class AntNode(val heuristic: TTPHeuristic, name: String, var visibility: Double = 1.0) extends NodeImpl {
    self: Node =>

    val pheromones = scala.collection.mutable.Map[Node, Double]()

    def pheromone(node: Node): Double =
      pheromones.getOrElseUpdate(node, 1.0)

    def changePheromone(node: Node, value: Double): Unit =
      pheromones(node) = value
      
    override def toString = "%s\tvis: %.1f\tpher: %s" format (name, visibility, pheromones.map(_ match { case (node, ph) => "%s = %.2f" format (node.toShortString, ph) }).mkString(", "))
    def toShortString = "%s" format (name)
  }

  override def toString: String = nodes.mkString("\n") // nodes.mkString("[\n\t", "\n\t", "\n]")
}

class NormalAntGraph extends AntGraph {
  type Node = AntNode
  
  override def addNode: Node = addNode()
  def addNode(heuristic: TTPHeuristic = NullHeuristic, name: String = "Null"): Node = {
    nodes ::= new AntNode(heuristic, name)
    nodes.head
  }
}

class MinMaxAntGraph(var min: Double, var max: Double) extends AntGraph {
  type Node = AntNode with MinMax

  trait MinMax extends AntNode {
    override def pheromone(node: Node): Double =
      pheromones.getOrElseUpdate(node, max)

    override def changePheromone(node: Node, value: Double): Unit =
      pheromones(node) = if (value > max) max else if (value < min) min else value
  }
  
  override def addNode: Node = addNode()
  def addNode(heuristic: TTPHeuristic = NullHeuristic, name: String = "Null"): Node = {
    nodes ::= new AntNode(heuristic, name) with MinMax
    nodes.head
  }
}