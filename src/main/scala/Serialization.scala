import java.io.{FileInputStream, FileOutputStream, 
                ObjectInputStream, ObjectOutputStream}

object Serialization {
  def objectWriter(filename:String) = {
    new ObjectOutputStream (new FileOutputStream(filename))
  }

  def objectReader(filename:String) = {
    new ObjectInputStream (new FileInputStream (filename))
  }

  def writeGraphSize(w:ObjectOutputStream, nodes:Int, edges:Int) = {
    w.writeObject((nodes, edges))
  }

  // writeNode and writeEdge are identical now but may diverge. 
  def writeNode(w:ObjectOutputStream, n:Node) = {
    w.writeObject(n)
  }

  def writeEdge(w:ObjectOutputStream, e:Edge) = {
    w.writeObject(e)
  }
  
  def readGraphSize(r:ObjectInputStream):(Int, Int) = {
    r.readObject().asInstanceOf[(Int, Int)]
  }

  def readNode[NodeT <: Node](r:ObjectInputStream):NodeT = {
    r.readObject().asInstanceOf[NodeT]
  }

  def readEdge[EdgeT <: Edge](r:ObjectInputStream):EdgeT = {
    r.readObject().asInstanceOf[EdgeT]
  }
}
