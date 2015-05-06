package com.kodeworks.possy

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Yen {
  type Path[Key] = (Double, List[Key])

  def kShortestPath[Key](lookup: Map[Key, List[(Double, Key)]], start: Key, sink: Key, K: Int) = {
    val graph: mutable.Map[Key, List[(Double, Key)]] = mutable.Map(lookup.toSeq: _*)
    val A: ListBuffer[(Double, List[Key])] = ListBuffer(Dijkstra.shortestPath(lookup, List(0d -> List(start)), sink, Set()))
    val B = ListBuffer[List[Key]]()
    for (k <- 1 to K) {
      var i = 0
      while (i < A(k - 1)._2.size - 1) {
        val spurNode: Key = A(k - 1)._2(i)
        val rootPath: List[Key] = A(k - 1)._2.slice(0, i)
        for (p <- A if rootPath == p._2.slice(0, i)) {
          val pi = p._2(i)
          val pi1 = p._2(i + 1)
          graph.put(pi, graph(pi).filter(_._2 != pi1))
        }
        for (rootPathNode <- rootPath.filter(_ != spurNode)) {
          graph.remove(rootPathNode) //TODO do I need to remove rootPathNode from all edge destinations as well? If so this is way to ineffective
        }
        val spurPath = Dijkstra.shortestPath(graph.toMap, List(0d -> List(spurNode)), sink, Set())
        val totalPath: List[Key] = rootPath ++ spurPath._2
        B.append(totalPath)
      }
    }
    /*
               // Add back the edges and nodes that were removed from the graph.
               restore edges to Graph;
               restore nodes in rootPath to Graph;

           if B is empty:
               // This handles the case of there being no spur paths, or no spur paths left.
               // This could happen if the spur paths have already been exhausted (added to A),
               // or there are no spur paths at all - such as when both the source and sink vertices
               // lie along a "dead end".
               break;
           // Sort the potential k-shortest paths by cost.
           B.sort();
           // Add the lowest cost path becomes the k-shortest path.
           A[k] = B[0];
           B.pop();

       return A;
     */
  }
}
