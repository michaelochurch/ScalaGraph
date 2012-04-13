object MakeTestData {
  val targetDir = "src/main/resources"

  def makeSmallGraphs():Unit = {
    val nodeIds = (0 to 4).map(i => Name.forString("small.node.%d".format(i))).toArray
    val edgeIds = (0 to 2).map(i => Name.forString("small.edge.%d".format(i))).toArray
    
    val nodes = nodeIds.zipWithIndex.map {
      case (nodeId, idx) => {
	val typ = if (idx % 2 == 0) "evenNode" else "oddNode"
	BaseNode(nodeId, typ, Map("x" -> idx.toString))
      }
    }
					 
    val edges = new Array[BaseEdge](3)
    edges(0) = BaseEdge(edgeIds(0), nodeIds(0), nodeIds(1), 
			"evenEdge", Map[String, String]())
    edges(1) = BaseEdge(edgeIds(1), nodeIds(0), nodeIds(3), 
			"oddEdge", Map[String, String]())
    edges(2) = BaseEdge(edgeIds(2), nodeIds(1), nodeIds(0), 
			"evenEdge", Map[String, String]())

    val smallGraph = new ResultGraph(nodes, edges)
    val smallGraphFilename = "%s/small.graph".format(targetDir)
    smallGraph.saveToFile(smallGraphFilename)

    // invalidGraph : has edges "to nowhere". 
    val invalidGraph = new ResultGraph(nodes.filter(_.id != nodeIds(3)), edges)
    val invalidGraphFilename = "%s/invalid.graph".format(targetDir)
    invalidGraph.saveToFile(invalidGraphFilename)
  }

  def makeLinearGraph():Unit = {
    val len = 100

    val nodeIds = (1 to len).map(i => Name.forString("linear.node.%d".format(i))).toArray
    val edgeIds = (1 to (len - 1)).map(i => 
      Name.forString("linear.edge.%d".format(i))).toArray
    
    val nodes = nodeIds.zipWithIndex.map {
      case (id, index) => 
	BaseNode(id, "intNode", Map[String, String]("x" -> index.toString))
    }

    val edges = edgeIds.zipWithIndex.map {
      case (id, index) =>
	BaseEdge(id, nodeIds(index), nodeIds(index + 1),
		 "intEdge", Map[String, String]("y" -> index.toString))
    }

    val linearGraph = new ResultGraph(nodes, edges)
    val linearGraphFilename = "%s/linear.graph".format(targetDir)
    linearGraph.saveToFile(linearGraphFilename)
  }

  def main(args:Array[String]) = {
    TestEase.inTest()
    makeSmallGraphs()
    makeLinearGraph()
  }
}
