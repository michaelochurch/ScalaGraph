import scala.collection.mutable

// TODO(michaelochurch): Either learn a Java test framework
// (e.g. JUnit) or build something like my 'expect macro'.

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
	nodes(i) = BaseNode("testNode", Map("x" -> i.toString))
      }
      edges(0) = BaseEdge(nodeId(0), nodeId(1), "testEdge")
      edges(1) = BaseEdge(nodeId(0), nodeId(3), "testEdge")
      edges(2) = BaseEdge(nodeId(1), nodeId(0), "testEdge")
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

  def testValidateFailureCase() = {
    try {
      graphs("invalid").validate()
      throw new Exception("supposed to fail")
    }
    catch {
      case (e:IllegalArgumentException) => // OK
    }
  }

  def runTests() = {
    setUp()
    
    testValidateGraph()
    testGetNode()
    testGetEdge()
    testOutEdges()
    testInEdges()
    testValidateFailureCase()

    println("ResultGraphTest: PASSED")
  }

  def main(args:Array[String]) = {
    runTests()
  }
}
