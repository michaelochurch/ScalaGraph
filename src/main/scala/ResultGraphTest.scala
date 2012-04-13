import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import TestEase._

object ResultGraphTest {
  def runTests() = {
    TestEase.inTest()

    val graphTester = new GraphTest(ResultGraph.basic)
    val graphs = graphTester.go()
    
    val graphsDir = "src/main/resources"
    val invalidFilename = "%s/invalid.graph".format(graphsDir)

    val invalidGraph = 
      ResultGraph.loadFromFile[BaseNode, BaseEdge](invalidFilename)

    def testValidation() = {
      graphs("small").validate()
      exnClass("ValidationException") {
        invalidGraph.validate()
      }
    }

    testValidation()
  }

  def main(args:Array[String] = Array()) = {
    runTests()
    println("ResultGraphTest: PASSED")
  }
}
