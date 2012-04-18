object MemoryGraphTest {
  def runTests() = {
    TestEase.inTest()

    // 1. Run the common graph tests.
    val graphTester = new GraphTest(MemoryGraph.basic)
    val graphs = graphTester.go()

    // 2. Run some tests specific to mutable graphs.
    def addNodeAndRetrieveIt() = {
      val g = MemoryGraph.basic
      val node1 = BaseNode("testType", Map("one" -> "1"))
      val nodeId1 = g.addNode(node1)
      assert(g.getNode(nodeId1) == Some(node1))
    }

    addNodeAndRetrieveIt()
  }

  def main(args:Array[String] = Array()) = {
    runTests()
    println("MemoryGraphTest: PASSED")
  }
}
