import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import TestEase._

//TODO(moc): This test should be abstracted to work on
//any Graph type. 

object ResultGraphTest {
  def runTests() = {
    val graphTester = new GraphTest(ResultGraph.basic)
    graphTester.go()
    
    // TODO(): test validation. 
    
    println("ResultGraphTest: PASSED")
  }

  def main(args:Array[String]) = {
    runTests()
  }
}
