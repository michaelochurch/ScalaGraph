import scala.collection.JavaConverters._

// Mutable in-memory index. Used for mutable graphs where we care
// about performance.

class Index[K <% Ordered[K], V] {
  // TODO(michaelochurch): replace with mutable.TreeMap when it exists
  private val data = new java.util.TreeMap[K, Set[V]]

  // Include (k, v) in the index. 
  def add(k:K, v:V):Unit = {
    if (data.containsKey(k)) {
      val vs = data.get(k)
      data.put(k, vs + v)
    } else {
      data.put(k, Set(v))
    }
  }

  // Remove (k, v) from the index. 
  def remove(k:K, v:V):Unit = {
    if (data.containsKey(k)) {
      val vs = data.get(k)
      if (vs == Set(v)) {
	data.remove(k)
      } else {
	data.put(k, vs - v)
      }
    }
  }

  // Remove (k, v) for all v from the index. 
  def removeAll(k:K):Unit = {
    data.remove(k)
  }

  // Get all v for which (k, v) is in the index. 
  def lookup(k:K):Set[V] = {
    if (data.containsKey(k)) data.get(k) else Set()
  }

  // Get all v for which there's a (k, v) in the index such the
  // range. Some(k) represents a bound and None represents no bound.
  def lookupRange(kLow:Option[K], kHigh:Option[K]):Iterable[V] = {
    val mutableSubmap = 
      (kLow, kHigh) match {
	case (None, None) => data
	case (None, Some(kH)) => data.headMap(kH)
	case (Some(kL), None) => data.tailMap(kL)
	case (Some(kL), Some(kH)) => data.subMap(kL, kH)
      }
    val submap = Map() ++ mutableSubmap.asScala 
    submap.flatMap({case (k, vs) => vs})
  }
}
