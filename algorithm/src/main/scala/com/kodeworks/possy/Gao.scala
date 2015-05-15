package com.kodeworks.possy

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

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
    var fibNode: FibHeapNode[Node] = null
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
      var removedEdges: ArrayBuffer[Edge] = null
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
            j += 1
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

  object PathCandidates {
    var app: Double = 1
  }

  class PathCandidates(shortest: Path) {
    var candidates = ArrayBuffer[ArrayBuffer[Path]]()
    var currentIdx = 0
    var currentPos = 0
    var exactResults = 0
    var idx = 0
    val shortestDistance = shortest.totalDistance
    addOneCandidatePath(shortest)


    def enough(k: Int, cur_num: Int, cur_len: Int, topks: ArrayBuffer[Path]): Boolean = {
      var same_len1 = 0
      var same_len2 = 0
      for (p <- topks
           if p.getLength == cur_len)
        same_len1 += 1

      for (ps <- candidates;
           p <- ps
           if p.getLength == cur_len) {
        same_len2 += 1
        same_len2 - 1
      }
      cur_num + same_len2 - same_len1 > k
    }

    def getCurrent: Path = {
      if (currentIdx >= candidates.size)
        return null
      var currentResults: ArrayBuffer[Path] = candidates(currentIdx)
      if (currentPos >= currentResults.size || currentResults.size == 0) {
        currentIdx += 1
        var break = false
        while (currentIdx < candidates.size && !break) {
          currentResults = candidates(currentIdx)
          if (currentResults.size != 0)
            break = true
          else
            currentIdx += 1
        }
        currentPos = 0
      }
      if (currentIdx == candidates.size) return null
      currentResults = candidates(currentIdx)
      val onePath: Path = currentResults(currentPos)
      currentPos += 1
      exactResults += 1
      if (Parameter.detail && idx >= 1)
        System.out.println(idx + " : " + onePath.toString)
      idx += 1
      onePath
    }

    def addOneCandidatePath(onePath: Path) {
      val idx = onePath.totalDistance - shortestDistance
      if (idx >= candidates.size) {
        var i = candidates.size - 1
        while (i < idx) {
          candidates.append(ArrayBuffer[Path]())
          i += 1
        }
      }
      var results = candidates(idx)
      for (cpath <- results
           if cpath.toString.equalsIgnoreCase(onePath.toString))
        return
      results.append(onePath)
    }

    def addOneCandidatePathWithoutTesting(onePath: Path) {
      for (ps <- candidates;
           p <- ps
           if p.isEqual(onePath)) return
      val idx = onePath.totalDistance - shortestDistance
      if (idx >= candidates.size) {
        var i = candidates.size - 1
        while (i < idx) {
          candidates.append(ArrayBuffer[Path]())
          i += 1
        }
        candidates(idx).append(onePath)
      }
    }

    def enoughResults(k: Int): Boolean = {
      val maxLen: Int = (((currentIdx + shortestDistance) * PathCandidates.app).toInt) - shortestDistance
      var sunResults: Int = candidates(currentIdx).size - currentPos
      var i: Int = currentIdx + 1
      while (i < maxLen && i < candidates.size) {
        sunResults = sunResults + candidates(i).size
        i += 1
      }
      ((exactResults + sunResults) > k)
    }

    def outPutRestResult(topk: Int, curNum: Int) {
      var cur_num = curNum
      if (!Parameter.detail) return
      val maxLen: Int = (((currentIdx + shortestDistance) * PathCandidates.app).toInt) - shortestDistance
      var count: Int = exactResults
      var results: ArrayBuffer[Path] = null
      var onePath: Path = null
      var i: Int = currentPos
      while (currentIdx < candidates.size && i < candidates(currentIdx).size) {
        if (cur_num > topk) return
        onePath = candidates(currentIdx)(i)
        System.out.println(count + " : " + onePath.toString)
        count += 1
        cur_num += 1
        i += 1
      }
      currentIdx += 1
      i = currentIdx
      while (i < maxLen && i < candidates.size) {
        results = candidates(i)
        var j: Int = 0
        while (j < results.size) {
          if (cur_num > topk) return
          onePath = results(j)
          System.out.println(count + " : " + onePath.toString)
          count += 1
          cur_num += 1
          j += 1
        }
        i += 1
      }
    }
  }

  object shortestPathTree {
    var OUT: Int = 1
    var IN: Int = 0
  }

  class ShortestPathTree(var dgraph: Graph, var rootID: Int, var direction: Int = 1) {
    var activeNodesList: ArrayBuffer[Node] = ArrayBuffer[Node]()
    var nodesFinished = mutable.Set[Node]()
    var fibHeap: FibHeap[Node] = new FibHeap[Node]
    var fibnodesHash = mutable.Map[AnyRef, AnyRef]()
    var costs = ArrayBuffer[Int]()
    var leafNodesList = ArrayBuffer[Node]()
    var mergeNode: Node = null
    var benefit = 0
    var maxLevel = 0
    var maxCost = 0
    var totalNodes = 0
    dgraph.clearForNextSPT
    var rootNode: Node = dgraph.getNodeById(rootID)
    rootNode.cost = 0

    def constructRevSPTInMem_Fib {
      var cnode: Node = null
      cnode = rootNode
      nodesFinished.add(cnode)
      var break = false
      while (cnode != null && !break) {
        extendInNodesInMemory_Fib(cnode)
        var n: FibHeapNode[Node] = null
        n = fibHeap.removeMin
        if (n == null) break = true
        else {
          cnode = n.getData
          cnode.fibNode = null
          nodesFinished.add(cnode)
        }
      }
      initalCost(dgraph.nodeNum)
      visitTree
    }

    def extendInNodesInMemory_Fib(cnode: Node) {
      var fromID = 0
      var nextCost = 1
      var fromNode: Node = null
      var nextEdge: Edge = null
      var rs = dgraph.getInEdge(cnode.id)
      for (edge <- rs) {
        fromID = edge.fromNode.id
        nextCost = edge.distance
        fromNode = dgraph.getNodeById(fromID)
        if (fromNode == null || !nodesFinished.contains(fromNode)) {
          if (fromNode.fibNode == null) {
            fromNode = dgraph.getNodeById(fromID)
            fromNode.cost = cnode.cost + nextCost
            nextEdge = new Edge
            nextEdge.fromNode = cnode
            nextEdge.toNode = fromNode
            nextEdge.distance = nextCost
            fromNode.preEdge = nextEdge
            cnode.addEdgeIntoSPT(nextEdge)
            val n: FibHeapNode[Node] = new FibHeapNode[Node](fromNode, fromNode.cost)
            fromNode.fibNode = n
            fibHeap.insert(n, n.getKey)
          }
          else if (fromNode.cost > cnode.cost + nextCost) {
            val pnodeID: Int = fromNode.getPreNodeID
            val pnode: Node = dgraph.getNodeById(pnodeID)
            nextEdge = pnode.getEdgeFromToNodeInSPT(fromID)
            pnode.removeEdgeInSPT(nextEdge)
            nextEdge.toNode = fromNode
            nextEdge.fromNode = cnode
            nextEdge.distance = nextCost
            fromNode.preEdge = nextEdge
            cnode.addEdgeIntoSPT(nextEdge)
            fromNode.cost = cnode.cost + nextCost
            fibHeap.decreaseKey(fromNode.fibNode, fromNode.cost)
          }
        }
      }
    }

    def initalCost(size: Int) {
      costs = ArrayBuffer.fill(size)(Int.MaxValue)
    }

    def getNodeWithMinCost: Node = {
      if (activeNodesList.isEmpty) return null
      activeNodesList.minBy(_.cost)
    }

    def extendInNodesInMemory(cnode: Node) {
      var fromID = 0
      var nextCost = 1
      var fromNode: Node = null
      var nextEdge: Edge = null
      var rs = dgraph.getInEdge(cnode.id)
      for (edge <- rs) {
        fromID = edge.fromNode.id
        nextCost = edge.distance
        fromNode = dgraph.getNodeById(fromID)
        if (fromNode == null || nodesFinished.contains(fromNode)) {
          if (!activeNodesList.contains(fromNode)) {
            fromNode = dgraph.getNodeById(fromID)
            fromNode.cost = cnode.cost + nextCost
            nextEdge = new Edge
            nextEdge.fromNode = cnode
            nextEdge.toNode = fromNode
            nextEdge.distance = nextCost
            fromNode.preEdge = nextEdge
            cnode.addEdgeIntoSPT(nextEdge)
            activeNodesList.append(fromNode)
          }
          else if (fromNode.cost > cnode.cost + nextCost) {
            val pnodeID: Int = fromNode.getPreNodeID
            val pnode: Node = dgraph.getNodeById(pnodeID)
            nextEdge = pnode.getEdgeFromToNodeInSPT(fromID)
            pnode.removeEdgeInSPT(nextEdge)
            nextEdge.toNode = fromNode
            nextEdge.fromNode = cnode
            nextEdge.distance = nextCost
            fromNode.preEdge = nextEdge
            cnode.addEdgeIntoSPT(nextEdge)
            fromNode.cost = cnode.cost + nextCost
          }
        }
      }
    }

    def visitTree: Int = {
      val wklist = mutable.Queue[Node]()
      wklist.enqueue(rootNode)
      rootNode.cost = 0
      rootNode.parent = -1
      rootNode.level = 1
      var cnt = 0
      var tempmaxLevel = 0
      var cnode: Node = null
      var edgeCnt = 0
      var cedge: Edge = null
      leafNodesList.clear
      totalNodes = 0
      while (!wklist.isEmpty) {
        cnode = wklist.dequeue
        if (maxCost < cnode.cost) {
          maxCost = cnode.cost
        }
        totalNodes += 1
        this.costs(cnode.id) = cnode.cost
        var childNode: Node = null
        edgeCnt = edgeCnt + cnode.edgesInSPT.size
        if (cnode.edgesInSPT.isEmpty) {
          leafNodesList.add(0, cnode)
        }
        {
          var i: Int = 0
          while (i < cnode.getEdgesInSPT.size) {
            {
              cedge = cnode.getEdgesInSPT.get(i)
              childNode = cedge.getToNode
              childNode.cost = cnode.cost + cedge.distance
              childNode.parent = cnode.id
              wklist.add(childNode)
              childNode.level = cnode.level + 1
              if (childNode.level > tempmaxLevel) {
                tempmaxLevel = childNode.level
              }
              cnt += 1
            }
            ({
              i += 1;
              i - 1
            })
          }
        }
      }
      this.maxLevel = tempmaxLevel
      return cnt
    }

    def getMaxCost: Int = {
      return maxCost
    }

    def getMaxHeight: Int = {
      return maxLevel
    }

    def getTotalNodes: Int = {
      return totalNodes
    }

    def getRoot: Node = {
      return rootNode
    }

    /**
     * make the pre, post, parent annotation on each node(post is not calculated?)
     * 第一个参数是pre, 第二个参数是-1;
     *
     */
    def makePrePostParentAnnotation {
      rootNode.annnotateNode(0, -1)
      return
    }
  }

}
