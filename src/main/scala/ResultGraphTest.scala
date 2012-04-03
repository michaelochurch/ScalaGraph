import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import TestEase._

object ResultGraphTest {
  type ResultGraphBase = ResultGraph[BaseNode, BaseEdge]

  val nodes = new Array[BaseNode](5)
  val edges = new Array[BaseEdge](3)

  def nodeId(i:Int) = nodes(i).id
  def edgeId(i:Int) = edges(i).id

  val graphs = mutable.Map[String, ResultGraphBase]()
  
  def setUp() = {
    def createNodesAndEdges() = {
      for(i <- 0 to 4) {
	val nodeType = if (i % 2 == 0) "evenNode" else "oddNode"
	nodes(i) = BaseNode(nodeType, Map("x" -> i.toString))
      }
      edges(0) = BaseEdge(nodeId(0), nodeId(1), "evenEdge")
      edges(1) = BaseEdge(nodeId(0), nodeId(3), "oddEdge")
      edges(2) = BaseEdge(nodeId(1), nodeId(0), "evenEdge")
    }
    
    createNodesAndEdges()
    // main: A valid graph with 5 nodes and 3 edges.
    graphs("main") = new ResultGraph(nodes, edges)
    // invalid: contains edges "to nowhere", i.e. with nodes not in the graph.
    graphs("invalid") = new ResultGraph(nodes.tail, edges)
  }

  def testValidateGraph() = {
    graphs("main").validate()
  }

  def testGetNode() = {
    for(i <- 0 to 4) {
      assert(graphs("main").getNode(nodeId(i)) == Some(nodes(i)))
    }
    val badId = Name.make()
    assert(graphs("main").getNode(badId) == None)
  }

  def testGetEdge() = {
    for(i <- 0 to 2) {
      assert(graphs("main").getEdge(edgeId(i)) == Some(edges(i)))
    }
    val badId = Name.make()
    assert(graphs("main").getNode(badId) == None)
  }

  def testOutEdges() = {
    assert(graphs("main").outEdges(nodeId(0)) == Set(edges(0), edges(1)))
    assert(graphs("main").outEdges(nodeId(1)) == Set(edges(2)))
    assert(graphs("main").outEdges(nodeId(3)) == Set())
  }

  def testInEdges() = {
    assert(graphs("main").inEdges(nodeId(0)) == Set(edges(2)))
    assert(graphs("main").inEdges(nodeId(3)) == Set(edges(1)))
    assert(graphs("main").inEdges(nodeId(4)) == Set())
  }

  def testSearch() = {
    // 1. NodeFilter TrueNF matches all nodes.
    assert(graphs("main").search(FindNodes(TrueNF)) ==
      new ResultGraph(nodes, Set.empty))

    // 2. NodeFilter FalseNF matches no nodes. 
    assert(graphs("main").search(FindNodes(FalseNF)) ==
      new ResultGraph(Set.empty, Set.empty))

    // 3. NodeIdIn returns nodes w/ matching IDs.
    val nfIds = NodeIdIn(Set(1, 2, 4).map(i => nodeId(i)))
    assert(graphs("main").search(FindNodes(nfIds)) == 
      new ResultGraph(Set(1, 2, 4).map(i => nodes(i)), Set.empty))

    val nfEven = NodeTypeIn("evenNode")
    assert(graphs("main").search(FindNodes(nfEven)) ==
      new ResultGraph(Set(0, 2, 4).map(i => nodes(i)), Set.empty))
  }

  def testValidateFailureCase() = {
    exnClass("ValidationException") {
      graphs("invalid").validate()
    }
  }

  def runTests() = {
    setUp()
    
    testValidateGraph()
    testValidateFailureCase()

    testGetNode()
    testGetEdge()
    testOutEdges()
    testInEdges()
    testSearch()

    println("ResultGraphTest: PASSED")
  }

  def main(args:Array[String]) = {
    runTests()
  }
}
