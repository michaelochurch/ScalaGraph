object Test {
  def addNodeAndRetrieveIt() = {
    val g = new MutableInMemoryGraph[BaseNode,BaseEdge]()
    val node1 = BaseNode("testType", Map("one" -> "1"))
    val nodeId1 = g.addNode(node1)
    assert(g.getNode(nodeId1) == Some(node1))
  }

  def runTests() = {
    addNodeAndRetrieveIt()
  }

  def main(args:Array[String]) = {
    runTests()
    println("Passed.")
  }
}
