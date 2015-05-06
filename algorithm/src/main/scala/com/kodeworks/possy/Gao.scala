package com.kodeworks.possy

import scala.collection.mutable.ArrayBuffer

/**
 * Created by eirirlar on 06.05.2015.
 */
object Gao {

  class Edge {
    var fromNode: Node = null
    var toNode: Node = null
    var distance: Int = 0
    var sideCost: Int = 0

    def setFromNode(fromNode1: Node) {
      fromNode = fromNode1
    }

    def setToNode1(toNode1: Node) {
      toNode = toNode1
    }

    def getToNodeID: Int = {
      return toNode.id
    }

    def getToNode: Node = {
      return toNode
    }

    def equal(edge1: Edge): Boolean = {
      if (edge1.fromNode.id == this.fromNode.id &&  edge1.toNode.id == this.toNode.id) {
        return true
      }
      else {
        return false
      }
    }

    def reverseEdge: Edge = {
      val redge: Edge = new Edge
      redge.fromNode = this.toNode
      redge.toNode = this.fromNode
      redge.distance = this.distance
      return redge
    }

    override def toString: String = {
      var content: String = ""
      content = " from=" + this.fromNode.id + " to=" + this.toNode.id + " distance=" + this.distance + " sidetrack=" + this.sideCost
      return content
    }
  }

  class Node {
      var id: Int = 0
      var fibNode: FibHeapNode[_] = null
      private var outEdgesInGraph = ArrayBuffer[Edge]()
      private var inEdgesInGraph = ArrayBuffer[Edge]()
      private var edgesInSPT = ArrayBuffer[Edge]()
      private[memory] var preEdge: Edge = null
      private[memory] var preEdgeSideCost: Edge = null
      var cost: Int = Int.MaxValue
      var level: Int = 0
      var pre: Int = 0
      var post: Int = 0
      var parent: Int = 0
      var sideCost: Int = 0
      var minSumSideCost: Int = Int.MaxValue
      var hop: Int = 0
      var treeLevel: Int = 0
      var inComingEdges: Int = 0

      def this(id1: Int) {
        this()
        id = id1
      }

      def addOutEdgeIntoGraph(edge1: Edge) {
        if (!outEdgesInGraph.contains(edge1)) {
          outEdgesInGraph.append(edge1)
        }
      }

      def addInEdgeIntoGraph(edge1: Edge) {
        if (!inEdgesInGraph.contains(edge1)) {
          inEdgesInGraph.append(edge1)
        }
      }

      def addEdgeIntoSPT(edge1: Edge) {
        if (!edgesInSPT.contains(edge1)) {
          edgesInSPT.append(edge1)
        }
      }

      def addEdge(toNode1:  Node, cost: Int) {
        val newEdge: Edge = new Edge
        newEdge.fromNode = this
        newEdge.toNode = toNode1
        newEdge.distance = cost
        this.addOutEdgeIntoGraph(newEdge)
        toNode1.addInEdgeIntoGraph(newEdge)
      }

      def addChildNode(childNode:  Node) {
        val  edge1: Edge = new Edge
         edge1.fromNode = this
         edge1.toNode = childNode
        this.addOutEdgeIntoGraph(edge1)
        childNode.addInEdgeIntoGraph(edge1)
      }

      def removeEdgeInSPT(edge1: Edge) {
        edgesInSPT.remove(edgesInSPT.indexOf(edge1))
      }

      def getTotalEdge: Int = {
        return outEdgesInGraph.size
      }

      def getToNodesString: Vector[_] = {
        val toNodes: Vector[_] = new Vector[_]
        var cedge: Edge = null
        {
          var i: Int = 0
          while (i < outEdgesInGraph.size) {
            {
              cedge = outEdgesInGraph.get(i)
              toNodes.append(String.valueOf(cedge.getToNode))
            }
            ({
              i += 1; i - 1
            })
          }
        }
        return toNodes
      }

      def getToNodesList: List[node] = {
        val toNodes: List[node] = new ArrayList[node]
        var cedge: Edge = null
        {
          var i: Int = 0
          while (i < outEdgesInGraph.size) {
            {
              cedge = outEdgesInGraph.get(i)
              toNodes.append(cedge.toNode)
            }
            ({
              i += 1; i - 1
            })
          }
        }
        return toNodes
      }

      def getToNodes: Array[Int] = {
        val toNodes: Array[Int] = new Array[Int](outEdgesInGraph.size)
        var cedge: Edge = null
        {
          var i: Int = 0
          while (i < outEdgesInGraph.size) {
            {
              cedge = outEdgesInGraph.get(i)
              toNodes(i) = cedge.getToNodeID
            }
            ({
              i += 1; i - 1
            })
          }
        }
        return toNodes
      }

      def getOutEdgesInGraph: List[edge] = {
        return outEdgesInGraph
      }

      def getInEdgesInGraph: List[edge] = {
        return inEdgesInGraph
      }

      def getedgesInSPT: List[edge] = {
        return edgesInSPT
      }

      def getParent: Int = {
        return parent
      }

      def getPreNodeID: Int = {
        val cedge: Edge = this.preEdge
        if (cedge == null) return -1
        if (cedge.fromNode eq this) return cedge.toNode.id
        else return cedge.fromNode.id
      }

      /**
       * 返回到给点节点的边
       * @param toNodeID int
       * @return Edge
       */
      def getEdgeFromToNodeInSPT(toNodeID: Int): Edge = {
        var cedge: Edge = null
        {
          var i: Int = 0
          while (i < edgesInSPT.size) {
            {
              cedge = edgesInSPT.get(i)
              if (cedge.getToNodeID == toNodeID) {
                return cedge
              }
            }
            ({
              i += 1; i - 1
            })
          }
        }
        return null
      }

      def annnotateNode(pre: Int, parent: Int): Int = {
        this.parent = parent
        var currentPre: Int = pre
        this.pre = currentPre
        var tNode:  Node = null
        {
          var i: Int = 0
          while (i < this.edgesInSPT.size) {
            {
              tNode = this.edgesInSPT.get(i).toNode
              currentPre += 1
              currentPre = tNode.annnotateNode(currentPre, this.id)
            }
            ({
              i += 1; i - 1
            })
          }
        }
        currentPre += 1
        this.post = currentPre
        return currentPre
      }

      def clearForNextSPT {
        edgesInSPT.clear
        var cedge: Edge = null
        {
          var i: Int = 0
          while (i < outEdgesInGraph.size) {
            {
              cedge = outEdgesInGraph(i)
              cedge.sideCost = 0
            }
            ({
              i += 1; i - 1
            })
          }
        }
        this.preEdge = null
        this.cost = Int.MaxValue
        this.hop = 0
      }

      def setSideCost {
        var cedge: Edge = null
        {
          var i: Int = 0
          while (i < outEdgesInGraph.size) {
            {
              cedge = outEdgesInGraph.get(i)
              cedge.sideCost = cedge.distance - this.cost + cedge.toNode.cost
            }
            ({
              i += 1; i - 1
            })
          }
        }
      }
    }

  }

}
