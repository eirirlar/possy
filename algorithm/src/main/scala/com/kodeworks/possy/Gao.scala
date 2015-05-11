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

  class Node(val id: Int) {
    var fibNode: FibHeapNode[_] = null
    var outEdgesInGraph = ArrayBuffer[Edge]()
    var inEdgesInGraph = ArrayBuffer[Edge]()
    var edgesInSPT = ArrayBuffer[Edge]()
    var preEdge: Edge = null
    var preEdgeSideCost: Edge = null
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

  class Path(val dgraph: Graph) {
    var edges = new ArrayBuffer[Edge]
    var totalDistance: Int = 0
    var sourceNode: Node = null
    var cnode: Node = null
    var pathID: Int = 0
    var cnodeSideCost: Int = 0
    Path.count += 1
    //TODO static id generator may cause problems
    pathID = Path.count

    def getLength = totalDistance

    def addEdgeIntoPath(cedge: Edge) {
      edges.append(cedge)
      totalDistance = getTotalDistance
    }

    def setSourceNode(sourceNode: Node) {
      this.sourceNode = sourceNode
    }

    def setCnode(cnode: Node) {
      this.cnode = cnode
    }

    def getTotalDistance: Int =
      edges.map(_.distance).sum

    def addEdgeFirst(cedge: Edge) {
      edges.prepend(cedge)
      //TODO why not add cedge.distance to totaldistance instead?
      totalDistance = this.getTotalDistance
    }

    def isEqual(second: Path): Boolean = {
      if (this.size != second.size || second.getLength != this.getLength) return false
      edges.zip(second.edges).forall(es => es._1.fromNode.id == es._2.fromNode.id
        && es._1.toNode.id == es._2.toNode.id
        && es._1.distance == es._2.distance)
    }

    override def toString: String = {
      var s = edges.map(_.fromNode.id).mkString("->")
      if (edges.nonEmpty) {
        s = s + "->" + edges.last.toNode.id
      }
      totalDistance = this.getTotalDistance
      s + " (cost=" + totalDistance + ")"
    }

    def getNextPaths(topks: ArrayBuffer[Path], toID: Int): ArrayBuffer[Path] = {
      val nexts = ArrayBuffer[Path]()
      val nodes = edges.map(_.fromNode)
      if (edges.nonEmpty) {
        nodes.append(edges.last.toNode)
      }
      var next: Path = null
      if (edges.size < 1) return nexts
      var removedEdges: List[Edge] = null
      var i = getStartIdx(topks)
      while (i < nodes.size - 1) {
        removedEdges = getRemovedEdges(topks, i)
        val sptsc = new ShortestPathTreeSideCost(dgraph, this, removedEdges, nodes(i).id, toID)
        next = sptsc.buildNextShortestPath
        if (next != null) {
          nexts.append(next)
          //TODO write static variable
          ShortestPathTreeSideCost.rtotalCandidates += 1
          if (ShortestPathTreeSideCost.maxThreshold < next.cnodeSideCost) {
            //TODO write static variable
            ShortestPathTreeSideCost.maxThreshold = next.cnodeSideCost
          }
        }
        sptsc.resetSideCost
        i += 1

      }
      return nexts
    }

    def getStartIdx(topks: ArrayBuffer[Path]): Int = {
      var cpath: Path = null
      var idx: Int = 0
      var cedge1: Edge = null
      var cedge2: Edge = null
      var i: Int = 0
      while (i < topks.size - 1) {
        cpath = topks(i)
        def getIdx(_idx: Int): Int = {
          var idx = _idx
          var j: Int = 0
          while (j < cpath.edges.size
            && j < size) {
            cedge1 = cpath.edges(j)
            cedge2 = get(j)
            if (cedge1.fromNode.id != cedge2.fromNode.id) return idx
            else if (j > idx) idx = j
            j += 1;
          }
          idx
        }
        idx = getIdx(idx)
        i += 1
      }
      idx
    }

    private def getRemovedEdges(topks: ArrayBuffer[Path], idx: Int): ArrayBuffer[Edge] = {
      var cpath: Path = null
      val nextEdges = ArrayBuffer[Edge]()
      var i: Int = 0
      while (i < topks.size) {
        cpath = topks(i)
        var j = 0
        var break = false
        while (j < cpath.size && j < idx && !break) {
          if (!cpath.get(j).equal(edges(j))) break = true
          else j += 1
        }
        if (j == idx
          && cpath.size > idx
          && !nextEdges.contains(cpath.get(idx)))
          nextEdges.append(cpath.get(idx))
        i += 1
      }
      nextEdges
    }

    def size = edges.size

    def get(idx: Int): Edge = {
      return edges(idx)
    }

    def isValidate: Boolean = {
      var cedge1: Edge = null
      var cedge2: Edge = null
      var i: Int = 0
      while (i < edges.size - 1) {
        cedge1 = edges(i)
        cedge2 = edges(i + 1)
        if (cedge1.toNode ne cedge2.fromNode) return false
        i += 1
      }
      true
    }
  }

  object Path {
    var count = 0
  }

  class Graph(var size: Int) {
    var nodes = ArrayBuffer.tabulate[Node](size)(new Node(_))

    def nodeNum = size

    def getOutEdge(fromID: Int): ArrayBuffer[Edge] =
      nodes(fromID).getOutEdgesInGraph

    def getInEdge(toID: Int): ArrayBuffer[Edge] =
      nodes(toID).getInEdgesInGraph

    def getNodes = nodes

    def getNodeById(id: Int): Node = nodes(id)

    def clearForNextSPT {
      nodes.foreach(_.clearForNextSPT)
    }

    def setSideCost {
      nodes.foreach(_.setSideCost)
    }

    def getShorestPath(fromNode: Int, toNode: Int): Path = {
      var fnode = nodes(fromNode)
      var tnode = nodes(toNode)
      val spath = new Path(this)
      var tmp: Node = fnode
      var cedge: Edge = null
      while (tmp.parent != -1) {
        cedge = new Edge
        cedge.fromNode = tmp
        cedge.toNode = tmp.preEdge.toNode
        cedge.distance = tmp.preEdge.distance
        tmp = getNodeById(tmp.parent)
        cedge.toNode = tmp
        spath.addEdgeIntoPath(cedge)
      }
      spath
    }

    def buildTopKPaths(fromNode: Int, toNode: Int, topk: Int, methodType: Int) {
      var spath: Path = getShorestPath(fromNode, toNode)
      spath.setSourceNode(this.getNodeById(fromNode))
      var paths = new TopKPaths(this, spath)
      if (Parameter.earlyTerminate)
        paths.buildTopKPathsEarly(topk, methodType)
      else
        paths.buildTopKPathsNormal(topk, methodType)
      spath = null
      paths = null
      //TODO wtf?!
      System.gc
    }

    def resetNodeCost {
      nodes.foreach(n => {
        n.cost = Int.MaxValue
        n.parent = -1
        n.preEdge = null
      })
    }
  }
}
