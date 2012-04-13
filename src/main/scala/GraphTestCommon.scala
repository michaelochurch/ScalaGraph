import scala.collection.mutable
import TestEase._

class GraphTest[GraphT <: Graph[_, _, GraphT]](empty:GraphT) {
  val graphsDir = "src/main/resources"
  TestEase.inTest()

  val graphs = mutable.Map[String, GraphT]() 

  def mkEmptyGraph():GraphT = 
    empty.getClass.newInstance.asInstanceOf[GraphT]

  def loadGraph(filename:String):GraphT = {
    mkEmptyGraph().loadFromFile(filename)
  }

  def setUp():Unit = {
    graphs("small") = loadGraph("%s/small.graph".format(graphsDir))
    graphs("invalid") = loadGraph("%s/invalid.graph".format(graphsDir))
    graphs("linear") = loadGraph("%s/linear.graph".format(graphsDir))
  }

  def serializationTest():Unit = {
    val filename = TempFile.name()
    graphs("small").saveToFile(filename)
    val g2 = loadGraph(filename)
    assert(graphs("small") == g2)
  }

  def smallGraphTests():Unit = {
    // There's a DRY violation between these tests and MakeTestData, but we 
    // want that, because it validates that the graph's getNode and getEdge
    // function as expected. It would be cheating to get them from the file. 
    val nodeIds = (0 to 4).map(i => 
      Name.forString("small.node.%d".format(i))).toArray
    val nodes = nodeIds.zipWithIndex.map {
      case (id, index) => {
	val nodeType = if (index % 2 == 0) "evenNode" else "oddNode"
	BaseNode(id, nodeType, Map("x" -> index.toString))
      }
    }
    
    val edgeIds = (0 to 2).map(i => Name.forString("small.edge.%d".format(i)))
    val edges = new Array[BaseEdge](3)
    edges(0) = BaseEdge(edgeIds(0), nodeIds(0), nodeIds(1), "evenEdge")
    edges(1) = BaseEdge(edgeIds(1), nodeIds(0), nodeIds(3), "oddEdge")
    edges(2) = BaseEdge(edgeIds(2), nodeIds(1), nodeIds(0), "evenEdge")

    def testGetNode() = {
      for (i <- 0 to 4) {
	assert(graphs("small").getNode(nodeIds(i)) ==
	  Some(nodes(i)))
      }
      
      val badId = Name.make()
      assert(graphs("small").getEdge(badId) == None)
    }

    def testGetEdge() = {
      for (i <- 0 to 2) {
	assert(graphs("small").getEdge(edgeIds(i)) ==
	  Some(edges(i)))
      }
      
      val badId = Name.make()
      assert(graphs("small").getEdge(badId) == None)
    }

    testGetNode()
    testGetEdge()
  }

  def go() = {
    setUp()
    serializationTest()
    smallGraphTests()
  }
}
