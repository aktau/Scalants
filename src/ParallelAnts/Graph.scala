package ParallelAnts

import scala.util.Random

abstract class Graph {
  type Node <: NodeIntf
  
  abstract class NodeIntf {
    def connectWith(node: Node) : Unit
    def connectWithAll : Unit
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
  	
    def connectWith(node: Node) : Unit = {
      if (!(neighbours contains node) && this != node)
	    	neighbours ::= node
      
      node.addReverseEdge(this)
    }
  	
  	def connectWithAll : Unit = 
  		nodes foreach { this connectWith _ }
  	
    private[NodeImpl] def addReverseEdge(node: Node) : Unit = 
    	if (!(neighbours contains node) && this != node) neighbours ::= node
  }
  
  protected def newNode: Node
  var nodes: List[Node] = Nil
  
  def addNode: Node = {
    nodes ::= newNode
    nodes.head
  }
  
  def connectAll : Unit =
  	nodes foreach { _.connectWithAll }
  
  def randomNode : Node =
  	nodes(Random.nextInt(nodes.length))
}

class AntGraph extends UndirectedGraph {
	type Node = AntNode
	
	class AntNode(val heuristic: TTPHeuristic, name: String, var visibility: Double = 1.0) extends NodeImpl {
		val pheromones = scala.collection.mutable.Map[Node,Double]()
		
		def pheromone(node: Node) : Double = 
			pheromones.getOrElseUpdate(node, 1.0)
			
		def changePheromone(node: Node, value: Double) : Unit = 
			pheromones(node) = value
			
		//override def toString = "%s -> (vis: %f) -> (pher: %s)" format (name, visibility, pheromones.keys map (_.toShortString) zip pheromones.values)
			override def toString = "%s -> (vis: %f) -> (pher: %s)" format (name, visibility, pheromones values)
		def toShortString = "%s" format (name)
	}
	
	protected def newNode: Node = newNode()
	protected def newNode(heuristic: TTPHeuristic = NullHeuristic, name: String = "Undefined") = 
		new AntNode(heuristic, name)
	
	def addNode(heuristic: TTPHeuristic = NullHeuristic, name: String) : Node = {
		nodes ::= newNode(heuristic, name)
		nodes.head
	}
	
	override def toString : String = nodes.mkString("\n") // nodes.mkString("[\n\t", "\n\t", "\n]")
}

class ConcreteUndirectedGraph extends UndirectedGraph {
  type Node = NodeImpl
  
  protected def newNode: Node = new NodeImpl
}