package com.kodeworks.possy

import scala.collection.mutable.ArrayBuffer

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
      if (edge1.fromNode.id == this.fromNode.id && edge1.toNode.id == this.toNode.id) {
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

    def addEdge(toNode1: Node, cost: Int) {
      val newEdge: Edge = new Edge
      newEdge.fromNode = this
      newEdge.toNode = toNode1
      newEdge.distance = cost
      this.addOutEdgeIntoGraph(newEdge)
      toNode1.addInEdgeIntoGraph(newEdge)
    }

    def addChildNode(childNode: Node) {
      val edge1: Edge = new Edge
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

    def getToNodesString: ArrayBuffer[String] = {
      outEdgesInGraph.map(e => String.valueOf(e.getToNode))
    }

    def getToNodesList: ArrayBuffer[Node] = {
      outEdgesInGraph.map(_.toNode)
    }

    def getToNodes: ArrayBuffer[Int] = {
      outEdgesInGraph.map(_.getToNodeID)
    }

    def getOutEdgesInGraph = outEdgesInGraph

    def getInEdgesInGraph = inEdgesInGraph

    def getedgesInSPT = edgesInSPT

    def getParent = parent

    def getPreNodeID: Int = {
      val cedge: Edge = this.preEdge
      if (cedge == null) return -1
      if (cedge.fromNode eq this) return cedge.toNode.id
      else return cedge.fromNode.id
    }

    def getEdgeFromToNodeInSPT(toNodeID: Int): Edge = {
      edgesInSPT.find(_.getToNodeID == toNodeID).getOrElse(null)
    }

    def annnotateNode(pre: Int, parent: Int): Int = {
      this.parent = parent
      var currentPre: Int = pre
      this.pre = currentPre
      edgesInSPT.foreach(e => {
        currentPre += 1
        currentPre = e.toNode.annnotateNode(currentPre, id)
      })
      currentPre += 1
      post = currentPre
      currentPre
    }

    def clearForNextSPT {
      edgesInSPT.clear
      outEdgesInGraph.foreach(_.sideCost = 0)
      preEdge = null
      cost = Int.MaxValue
      hop = 0
    }

    def setSideCost {
      outEdgesInGraph.foreach(e => e.sideCost = e.distance - cost + e.toNode.cost)
    }
  }

}
