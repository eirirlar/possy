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
      val nodes = edges.map(_.fromNode)
      if (edges.nonEmpty) {
        nodes.append(edges.last.toNode)
      }
      var next: Path = null
      if (edges.size < 1) return nexts
      var removedEdges: List[Edge] = null
      val startIdx: Int = this.getStartIdx(topks) {
        var i: Int = startIdx
        while (i < nodes.size - 1) {
          {
            removedEdges = getRemovedEdges(topks, i)
            val sptsc: shortestPathTreeSideCost = new shortestPathTreeSideCost(dgraph, this, removedEdges, nodes.get(i).id, toID)
            next = sptsc.buildNextShortestPath
            if (next != null) {
              nexts.add(next)
              shortestPathTreeSideCost.rtotalCandidates += 1
              if (shortestPathTreeSideCost.maxThreshold < next.cnodeSideCost) {
                shortestPathTreeSideCost.maxThreshold = next.cnodeSideCost
              }
            }
            sptsc.resetSideCost
          }
          ({
            i += 1;
            i - 1
          })
        }
      }
      return nexts
    }

    /**
     * 每次路径未必都从source开始算，我们获取正确的，减少重复计算的启动点
     * 什么是正确的启动点
     * top 1的source node设置为开始的节点
     * 那么，在所有确定的topk路径中，source node前面的节点实际上都以及处理过了
     * 我们应该扫描现有的topk的路径，和当前路径，发现当前路径中最靠后的，和现有路径重复的，现有topk路径
     * source node之前的节点，作为我们正确的启动节点
     * @param topks List
     * @return int
     */
    private def getStartIdx(topks: List[Path]): Int = {
      var cpath: Path = null
      var idx: Int = 0
      var cedge1: Edge = null
      var cedge2: Edge = null {
        var i: Int = 0
        while (i < topks.size - 1) {
          {
            cpath = topks.get(i) {
              var j: Int = 0
              while (j < cpath.edges.size && j < this.size) {
                {
                  cedge1 = cpath.edges.get(j)
                  cedge2 = this.get(j)
                  if (cedge1.fromNode.id != cedge2.fromNode.id) {
                    break //todo: break is not supported
                  }
                  else {
                    if (j > idx) {
                      idx = j
                    }
                  }
                }
                ({
                  j += 1;
                  j - 1
                })
              }
            }
          }
          ({
            i += 1;
            i - 1
          })
        }
      }
      return idx
    }

    private def getRemovedEdges(topks: List[Path], idx: Int): List[Edge] = {
      var cpath: Path = null
      val nextEdges: List[Edge] = new ArrayList[Edge]
      var j: Int = 0 {
        var i: Int = 0
        while (i < topks.size) {
          {
            cpath = topks.get(i) {
              j = 0
              while (j < cpath.size && j < idx) {
                {
                  if (!cpath.get(j).equal(edges.get(j))) {
                    break //todo: break is not supported
                  }
                }
                ({
                  j += 1;
                  j - 1
                })
              }
            }
            if (j == idx && cpath.size > idx) {
              if (!nextEdges.contains(cpath.get(idx))) {
                nextEdges.add(cpath.get(idx))
              }
            }
          }
          ({
            i += 1;
            i - 1
          })
        }
      }
      return nextEdges
    }

    def size: Int = {
      return edges.size
    }

    def get(idx: Int): Edge = {
      return edges.get(idx)
    }

    def isValidate: Boolean = {
      var cedge1: Edge = null
      var cedge2: Edge = null {
        var i: Int = 0
        while (i < edges.size - 1) {
          {
            cedge1 = edges.get(i)
            cedge2 = edges.get(i + 1)
            if (cedge1.toNode ne cedge2.fromNode) {
              return false
            }
          }
          ({
            i += 1;
            i - 1
          })
        }
      }
      return true
    }
  }

}

object Path {
  var count = 0
}

class Graph(val size: Int) {
  var nodes = ArrayBuffer[Node]()
  var size: Int = 0

  def nodeNum: Int = {
    return size
  }

  def this(size1: Int) {
    this()
    {
      var i: Int = 0
      while (i < size1) {
        {
          nodes.add(new node(i))
        }
        ({
          i += 1;
          i - 1
        })
      }
    }
    size = size1
  }

  def constructEdgsFromDB(sta: Statement, prefix: String) {
    try {
      val SQLState: String = " select fromnode, tonode, cost from " + prefix + "edge"
      var rs: ResultSet = null
      try {
        rs = sta.executeQuery(SQLState)
      }
      catch {
        case e1: Exception => {
          database.restartConnection
          rs = database.sta.executeQuery(SQLState)
        }
      }
      var fromID: String = null
      var toID: String = null
      var cost: Int = 0
      var cnode: Node = null
      while (rs.next) {
        fromID = rs.getString("fromNode").trim
        toID = rs.getString("toNode").trim
        cost = rs.getInt("cost")
        var ID: Integer = new Integer(fromID.trim)
        cnode = nodes.get(ID.intValue)
        ID = new Integer(toID.trim)
        cnode.addEdge(nodes.get(ID.intValue), cost)
      }
    }
    catch {
      case e: Exception => {
        e.printStackTrace(System.out)
      }
    }
  }

  /**
   * 返回一个节点相邻的边
   * @param fromID String
   * @return List
   */
  def getOutEdge(fromID: Int): List[Edge] = {
    val cnode: Node = nodes.get(fromID)
    return cnode.getOutEdgesInGraph
  }

  def getInEdge(toID: Int): List[Edge] = {
    val cnode: Node = nodes.get(toID)
    return cnode.getInEdgesInGraph
  }

  def getNodes: List[Node] = {
    return nodes
  }

  def getNodeById(id: Int): Node = {
    if (nodes == null) return null
    return nodes.get(id)
  }

  def clearForNextSPT {
    var cnode: Node = null {
      var i: Int = 0
      while (i < nodes.size) {
        {
          cnode = nodes.get(i)
          cnode.clearForNextSPT
        }
        ({
          i += 1;
          i - 1
        })
      }
    }
  }

  /**
   * set sideCost to every node, according to Eppstein.
   */
  def setSideCost {
    var cnode: Node = null {
      var i: Int = 0
      while (i < nodes.size) {
        {
          cnode = nodes.get(i)
          cnode.setSideCost
        }
        ({
          i += 1;
          i - 1
        })
      }
    }
  }

  def getShorestPath(fromNode: Int, toNode: Int): Path = {
    var fnode: Node = null
    var tnode: Node = null
    fnode = nodes.get(fromNode)
    tnode = nodes.get(toNode)
    val spath: Path = new path(this)
    var tmp: Node = fnode
    var cedge: Edge = null
    while (tmp.parent != -1) {
      cedge = new edge
      cedge.fromNode = tmp
      cedge.toNode = tmp.preEdge.toNode
      cedge.distance = tmp.preEdge.distance
      tmp = this.getNodeById(tmp.parent)
      cedge.toNode = tmp
      spath.addEdgeIntoPath(cedge)
    }
    return spath
  }

  def buildTopKPaths(fromNode: Int, toNode: Int, topk: Int, methodType: Int) {
    var spath: Path = this.getShorestPath(fromNode, toNode)
    spath.setSourceNode(this.getNodeById(fromNode))
    var paths: topKPaths = new topKPaths(this, spath)
    if (parameter.earlyTerminate) {
      paths.buildTopKPathsEarly(topk, methodType)
    }
    else {
      paths.buildTopKPathsNormal(topk, methodType)
    }
    spath = null
    paths = null
    System.gc
  }

  def resetNodeCost {
    var cnode: Node = null {
      var i: Int = 0
      while (i < nodes.size) {
        {
          cnode = nodes.get(i)
          cnode.cost = Integer.MAX_VALUE
          cnode.parent = -1
          cnode.preEdge = null
        }
        ({
          i += 1;
          i - 1
        })
      }
    }
  }
}

}
