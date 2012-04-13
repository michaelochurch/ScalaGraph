import scala.collection.mutable
import TestEase._

class GraphTest[GraphT <: Graph[BaseNode, BaseEdge, GraphT]](empty:GraphT) {
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

    def testOutEdges() = {
      assert(graphs("small").outEdges(nodeIds(0)) == Set(edges(0), edges(1)))
      assert(graphs("small").outEdges(nodeIds(1)) == Set(edges(2)))
      assert(graphs("small").outEdges(nodeIds(3)) == Set())
    }

    def testInEdges() = {
      assert(graphs("small").inEdges(nodeIds(0)) == Set(edges(2)))
      assert(graphs("small").inEdges(nodeIds(3)) == Set(edges(1)))
      assert(graphs("small").inEdges(nodeIds(4)) == Set())
    }

    def testSearchNodesOnly() = {
      // 1. NodeFilter TrueNF matches all nodes.
      assert(graphs("small").search(FindNodes(TrueNF)) ==
         new ResultGraph(nodes, Set.empty))
      
      // 2. NodeFilter FalseNF matches no nodes. 
      assert(graphs("small").search(FindNodes(FalseNF)) ==
         new ResultGraph(Set.empty, Set.empty))

      // 3. NodeIdIn returns nodes w/ matching IDs.
      val nfIds = NodeIdIn(Set(1, 2, 4).map(i => nodeIds(i)))
      assert(graphs("small").search(FindNodes(nfIds)) == 
        new ResultGraph(Set(1, 2, 4).map(i => nodes(i)), Set.empty))

     // 4. NodeTypeIn returns nodes of matching types. 
      val nfEven = NodeTypeIn("evenNode")
      assert(graphs("small").search(FindNodes(nfEven)) ==
          new ResultGraph(Set(0, 2, 4).map(i => nodes(i)), Set.empty))
    }

    def testSearchNodesAndEdges() = {
       val nfOdd = NodeTypeIn("oddNode")
       val efEven = EdgeTypeIn("evenEdge")
      
       // 1. Odd Nodes |-> Odd Edges => {N1, N3, N0 | E2}
       assert(graphs("small").search(
          FollowEdges(FindNodes(nfOdd), efEven)
       ) == new ResultGraph(Set(0, 1, 3).map(i => nodes(i)),
                             Set(edges(2))))

      // 2. Node N1 |-> all Edges => {N0, N1 | E2}
      val nfId1 = NodeIdIn(Set(nodeIds(1)))
      assert(graphs("small").search(FollowEdges(FindNodes(nfId1))) ==
         new ResultGraph(Set(0, 1).map(i => nodes(i)),
                         Set(edges(2))))

      // 3. Same search but w/ 2-ply FollowEdges => {N0, N1, N3 | E2, E1, E0}
      assert(graphs("small").search(
         FollowEdges(FindNodes(nfId1), TrueEF, TrueNF, Some(2))
      ) == new ResultGraph(Set(0, 1, 3).map(i => nodes(i)),
                           Set(0, 1, 2).map(i => edges(i))))

      // 4. Same search with no ply limit => same. (Termination test.)
      assert(graphs("small").search(
         FollowEdges(FindNodes(nfId1), TrueEF, TrueNF, None)
      ) == new ResultGraph(Set(0, 1, 3).map(i => nodes(i)),
                           Set(0, 1, 2).map(i => edges(i))))
      
      // TODO(): Test ~100-ply on linear graph. 
  }

    def testSearch() = {
      testSearchNodesOnly()
      testSearchNodesAndEdges()
    }

    testGetNode()
    testGetEdge()
    testOutEdges()
    testInEdges()
    testSearch()
  }

  // returns the graphs that were tested, for further (graph-specific)
  // testing if needed. 
  def go():Map[String, GraphT] = {
    setUp()
    serializationTest()
    smallGraphTests()
    Map() ++ graphs
  }
}
