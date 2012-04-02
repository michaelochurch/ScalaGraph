// TODO(michaelochurch): Either learn a Java test framework
// (e.g. JUnit) or build something like my 'expect macro'.

object ResultGraphTest {
  type ResultGraphBase = ResultGraph[BaseNode, BaseEdge]

  val nodes = new Array[BaseNode](5)
  val edges = new Array[BaseEdge](3)

  def nodeId(i:Int) = nodes(i).id
  def edgeId(i:Int) = edges(i).id

  def createNodesAndEdges() = {
    for(i <- 0 to 4) {
      nodes(i) = BaseNode("testNode", Map("x" -> i.toString))
    }
    edges(0) = BaseEdge(nodeId(0), nodeId(1), "testEdge")
    edges(1) = BaseEdge(nodeId(0), nodeId(3), "testEdge")
    edges(2) = BaseEdge(nodeId(1), nodeId(0), "testEdge")
  }

  def validateGraph(graph:ResultGraphBase) = {
    graph.validate()
  }

  def testGetNode(graph:ResultGraphBase) = {
    for(i <- 0 to 4) {
      assert(graph.getNode(nodeId(i)) == Some(nodes(i)))
    }
    val badId = Name.make()
    assert(graph.getNode(badId) == None)
  }

  def testGetEdge(graph:ResultGraphBase) = {
    for(i <- 0 to 2) {
      assert(graph.getEdge(edgeId(i)) == Some(edges(i)))
    }
    val badId = Name.make()
    assert(graph.getNode(badId) == None)
  }

  def testOutEdges(graph:ResultGraphBase) = {
    assert(graph.outEdges(nodeId(0)) == Set(edges(0), edges(1)))
    assert(graph.outEdges(nodeId(1)) == Set(edges(2)))
    assert(graph.outEdges(nodeId(3)) == Set())
  }

  def testInEdges(graph:ResultGraphBase) = {
    assert(graph.inEdges(nodeId(0)) == Set(edges(2)))
    assert(graph.inEdges(nodeId(3)) == Set(edges(1)))
    assert(graph.inEdges(nodeId(4)) == Set())
  }

  def testValidateFailCase() = {
    try {
      // badGraph is bad because it has edges whose nodes aren't in it. 
      val badGraph = new ResultGraph(nodes.tail, edges)
      badGraph.validate()
      throw new Exception("supposed to fail")
    }
    catch {
      case (e:IllegalArgumentException) => // OK
    }
  }

  def runTests() = {
    createNodesAndEdges()
    val graph = new ResultGraph(nodes, edges)
    validateGraph(graph)
    testGetNode(graph)
    testGetEdge(graph)
    testOutEdges(graph)
    testInEdges(graph)
    testValidateFailCase()
    println("ResultGraphTest: PASSED")
  }

  def main(args:Array[String]) = {
    runTests()
  }
}
