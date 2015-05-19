package com.kodeworks.possy

import java.text.SimpleDateFormat
import java.util.Date

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Gao {

  class Edge {
    var fromNode: Node = null
    var toNode: Node = null
    var distance: Int = 0
    var sideCost: Int = 0

     def equal(edge1: Edge): Boolean = {
      if (edge1.fromNode.id == fromNode.id && edge1.toNode.id == toNode.id) {
        return true
      }
      else {
        return false
      }
    }

    def reverseEdge: Edge = {
      val redge: Edge = new Edge
      redge.fromNode = toNode
      redge.toNode = fromNode
      redge.distance = distance
      return redge
    }

    override def toString: String = {
      var content: String = ""
      content = " from=" + fromNode.id + " to=" + this.toNode.id + " distance=" + this.distance + " sidetrack=" + this.sideCost
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
      outEdgesInGraph.map(e => String.valueOf(e.toNode.id))
    }

    def getToNodesList: ArrayBuffer[Node] = {
      outEdgesInGraph.map(_.toNode)
    }

    def getToNodes: ArrayBuffer[Int] = {
      outEdgesInGraph.map(_.toNode.id)
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
      edgesInSPT.find(_.toNode.id == toNodeID).getOrElse(null)
    }

    def annotateNode(pre: Int, parent: Int): Int = {
      this.parent = parent
      var currentPre: Int = pre
      this.pre = currentPre
      edgesInSPT.foreach(e => {
        currentPre += 1
        currentPre = e.toNode.annotateNode(currentPre, id)
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
      for (cpath <- topks) {
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

    def getShortestPath(fromNode: Int, toNode: Int): Path = {
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

    def buildTopKPaths(fromNode: Int, toNode: Int, topk: Int, methodType: Int): ArrayBuffer[Path] = {
      var spath: Path = getShortestPath(fromNode, toNode)
      spath.setSourceNode(this.getNodeById(fromNode))
      var paths = new TopKPaths(this, spath)
      var ret: ArrayBuffer[Path] =
        if (Parameter.earlyTerminate)
          paths.buildTopKPathsEarly(topk, methodType)
        else
          paths.buildTopKPathsNormal(topk, methodType)
      spath = null
      paths = null
      //TODO wtf?!
      System.gc
      ret
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

  object ShortestPathTree {
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
        if (cnode.edgesInSPT.isEmpty)
          leafNodesList.prepend(cnode)
        for (cedge <- cnode.edgesInSPT) {
          childNode = cedge.toNode
          childNode.cost = cnode.cost + cedge.distance
          childNode.parent = cnode.id
          wklist.enqueue(childNode)
          childNode.level = cnode.level + 1
          if (childNode.level > tempmaxLevel) {
            tempmaxLevel = childNode.level
          }
          cnt += 1
        }
      }
      this.maxLevel = tempmaxLevel
      cnt
    }

    def getMaxCost = maxCost

    def getMaxHeight = maxLevel

    def getTotalNodes = totalNodes

    def getRoot = rootNode

    def makePrePostParentAnnotation {
      rootNode.annotateNode(0, -1)
    }
  }

  object Parameter {
    val detail = false
    var earlyTerminate = false
    var pruningNodes = false
    val topks = 10
  }

  class ShortestPathTreeSideCost(
                                  var dgraph: Graph,
                                  var selected: Path,
                                  var removedEdges: ArrayBuffer[Edge],
                                  var sourceID: Int,
                                  var targetID: Int) {

    import ShortestPathTreeSideCost._

    var activeNodesList = ArrayBuffer[Node]()
    var nodesFinished = ArrayBuffer[Node]()
    var removedNodes = ArrayBuffer[Node]()
    var fibHeap: FibHeap[Node] = new FibHeap[Node]
    var costs: Array[Int] = null
    var leafNodesList = ArrayBuffer[Node]()
    var initValue = 0
    var sourceNode = dgraph.getNodeById(sourceID)
    var targetNode = dgraph.getNodeById(targetID)

    import scala.util.control.Breaks._

    breakable {
      for (cedge <- selected.edges) {
        removedNodes.append(cedge.fromNode)
        if (cedge.fromNode eq sourceNode) break
        else initValue = initValue + cedge.sideCost
      }
    }
    sourceNode.sideCost = initValue

    def getNodeWithMinCost: Node = {
      if (activeNodesList.isEmpty) return null
      val nextNode = activeNodesList.head
      activeNodesList.find(_.sideCost < nextNode.sideCost).getOrElse(nextNode)
    }

    private def isValidateCandidate(cnode: Node): Boolean = !removedNodes.contains(cnode)

    def resetSideCost {
      activeNodesList.foreach(_.sideCost = Int.MaxValue)
      nodesFinished.foreach(_.sideCost = Int.MaxValue)
    }

    def extendNodesInMemory_Fib(cnode: Node) {
      var toID = 0
      var nextCost = 1
      var toNode: Node = null
      val nextEdge: Edge = null
      var rs: ArrayBuffer[Edge] = dgraph.getOutEdge(cnode.id)
      for (edge <- rs
           if !isRemovedNextEdge(edge)) {
        toID = edge.toNode.id
        nextCost = edge.sideCost
        toNode = dgraph.getNodeById(toID)
        if (isValidateCandidate(toNode)) {
          if (toNode != null && nodesFinished.contains(toNode)) {
            if (toNode.treeLevel > cnode.treeLevel)
              toNode.inComingEdges += 1
          } else {
            if (toNode.fibNode == null) {
              toNode = dgraph.getNodeById(toID)
              toNode.preEdgeSideCost = edge
              toNode.sideCost = cnode.sideCost + nextCost
              if (toNode.sideCost < 0) {
                toNode.sideCost = Int.MaxValue
              }
              toNode.treeLevel = cnode.treeLevel + 1
              toNode.inComingEdges = 1
              if (toNode.sideCost <= sideCostThreshold) {
                val n = new FibHeapNode[Node](toNode, toNode.sideCost)
                toNode.fibNode = n
                fibHeap.insert(n, n.getKey)
              }
            }
            else if (toNode.sideCost > cnode.sideCost + nextCost) {
              val pnodeID = toNode.preEdgeSideCost.fromNode.id
              toNode.preEdgeSideCost = edge
              toNode.treeLevel = cnode.treeLevel + 1
              toNode.sideCost = cnode.sideCost + nextCost
              fibHeap.decreaseKey(toNode.fibNode, toNode.sideCost)
            }
          }
        }
      }
    }

    def buildNextShortestPath: Path = {
      var cnode = sourceNode
      val n = new FibHeapNode[Node](cnode, cnode.sideCost)
      cnode.fibNode = n
      fibHeap.insert(n, n.getKey)
      var next: Path = null
      var firstTime = 0

      var ret: Path = null
      var doRet = false

      def hwile {
        while (cnode != null) {
          extendNodesInMemory_Fib(cnode)
          fibHeap.delete(cnode.fibNode)
          cnode.fibNode = null
          nodesFinished.append(cnode)
          if (fibHeap.min != null)
            cnode = fibHeap.min.getData
          else cnode = null
          if (cnode == null) {
            while (fibHeap.isEmpty == false) {
              fibHeap.min.getData.fibNode = null
              fibHeap.removeMin
            }
            ret = next
            doRet = true
            return
          }
          if (this.isTerminate(cnode)) {
            if (firstTime == 0) {
              next = this.generatePath(cnode)
              if (next != null) {
                next.cnodeSideCost = cnode.sideCost
                firstTime += 1
              }
            }
            if (Parameter.pruningNodes) {
              if (EL == 1) {
                if (rtotalCandidates == Parameter.topks) {
                  if (ShortestPathTreeSideCost.sideCostThreshold > maxThreshold) {
                    ShortestPathTreeSideCost.sideCostThreshold = maxThreshold
                  }
                  rtotalCandidates = 0
                  maxThreshold = 0
                }
                return
              }
              if (EL == 0) {
                if (totalCandidates >= Parameter.topks) {
                  return
                }
                else {
                  totalCandidates = totalCandidates + this.getIncomingEdgeCombations(cnode)
                  if (cnode.sideCost > maxThreshold) {
                    maxThreshold = cnode.sideCost
                  }
                  if (totalCandidates >= Parameter.topks) {
                    if (ShortestPathTreeSideCost.sideCostThreshold > maxThreshold) {
                      ShortestPathTreeSideCost.sideCostThreshold = maxThreshold
                    }
                  }
                }
              }
            }
            else return
          }
        }
      }
      hwile
      if (doRet) return ret

      if (this.isTerminate(cnode) && next == null) {
        next = this.generatePath(cnode)
        if (next != null) {
          next.cnodeSideCost = cnode.sideCost
        }
      }
      while (fibHeap.isEmpty == false) {
        fibHeap.min.getData.fibNode = null
        fibHeap.removeMin
      }
      next
    }

    private def isTerminate(cnode: Node): Boolean = {
      if (!(cnode.pre >= targetNode.pre && cnode.post <= targetNode.post)) return false
      removedNodes.forall(rnode => !(cnode.pre >= rnode.pre && cnode.post <= rnode.post))
    }

    def generatePath(cnode: Node): Path = {
      Path.count += 1
      if (cnode == null || (sourceNode eq cnode)) return null
      val spath: Path = new Path(dgraph)
      selected.edges.takeWhile(_.fromNode ne sourceNode).foreach(spath.addEdgeIntoPath _)
      spath.setSourceNode(sourceNode)
      val second: Path = new Path(dgraph)
      var tmp: Edge = cnode.preEdgeSideCost
      while (tmp.fromNode ne sourceNode) {
        second.addEdgeFirst(tmp)
        tmp = tmp.fromNode.preEdgeSideCost
      }
      spath.addEdgeIntoPath(tmp)
      spath.edges.foreach(spath.addEdgeIntoPath _)
      spath.setCnode(cnode)
      tmp = cnode.preEdge
      if (tmp != null) {
        tmp = tmp.reverseEdge
        while (tmp.toNode.id != targetID) {
          spath.addEdgeIntoPath(tmp)
          tmp = tmp.toNode.preEdge
          if (tmp == null) tmp = null
          tmp = tmp.reverseEdge
        }
        spath.addEdgeIntoPath(tmp)
      }
      else if (cnode.id != targetNode.id)
        return null
      if (!spath.isValidate) {
        //TODO what am I supposed to do with this?
        System.out.println("current wrong path is " + spath.toString)
      }
      return spath
    }

    private def isRemovedNextEdge(cedge: Edge): Boolean =
      removedEdges.find(_.equal(cedge)).nonEmpty

    def setSideCostTreshold(maximalCost: Int) {
      sideCostThreshold = maximalCost
    }

    def extendNodesInMemory(cnode: Node) {
      var toID = 0
      var nextCost = 1
      var toNode: Node = null
      val nextEdge: Edge = null
      var rs: ArrayBuffer[Edge] = null
      rs = dgraph.getOutEdge(cnode.id)

      for (edge <- rs) {
        if (!isRemovedNextEdge(edge)) {
          toID = edge.toNode.id
          nextCost = edge.sideCost
          toNode = dgraph.getNodeById(toID)
          if (!this.isValidateCandidate(toNode)) {
            if (toNode != null && nodesFinished.contains(toNode)) {
              if (toNode.treeLevel > cnode.treeLevel)
                toNode.inComingEdges += 1
            } else {
              if (!activeNodesList.contains(toNode)) {
                toNode = dgraph.getNodeById(toID)
                toNode.preEdgeSideCost = edge
                toNode.sideCost = cnode.sideCost + nextCost
                if (toNode.sideCost < 0) {
                  toNode.sideCost = Int.MaxValue
                }
                toNode.treeLevel = cnode.treeLevel + 1
                toNode.inComingEdges = 1
                if (toNode.sideCost <= sideCostThreshold) {
                  activeNodesList.append(toNode)
                }
              }
              else if (toNode.sideCost > cnode.sideCost + nextCost) {
                val pnodeID = toNode.preEdgeSideCost.fromNode.id
                toNode.preEdgeSideCost = edge
                toNode.treeLevel = cnode.treeLevel + 1
                toNode.sideCost = cnode.sideCost + nextCost
              }
            }
          }
        }
      }
    }

    private def getIncomingEdgeCombations(cnode: Node): Int = {
      var tmp: Edge = cnode.preEdgeSideCost
      var before: Node = tmp.fromNode
      if (before.pre > cnode.pre && before.post < cnode.post)
        return 0
      while (before ne sourceNode) {
        if (before.pre < cnode.pre && before.post > cnode.post) {
          return 0
        }
        tmp = before.preEdgeSideCost
        before = tmp.fromNode
      }
      return 1
    }

  }

  object ShortestPathTreeSideCost {
    var sideCostThreshold = Int.MaxValue
    var maxThreshold = 0
    var totalCandidates = 0
    var rtotalCandidates = 0
    var EL = 0
    var searchedNodes = 0

    // TODO the fact that this is required prevents parallell execution
    def resetStaticForNextTime {
      sideCostThreshold = Int.MaxValue
      maxThreshold = 0
      totalCandidates = 0
    }
  }

  object TopKPaths {
    //TODO combine yen and eppstein flag?
    var COMBINEYENEPS: Int = 1
  }

  class TopKPaths(
                   var dgraph: Graph,
                   var shortestOne: Path) {

    import TopKPaths._

    var topks = ArrayBuffer[Path]()
    var candidates = ArrayBuffer[Path]()
    var toID = 0

    if (shortestOne.size > 0) {
      val last: Edge = shortestOne.get(shortestOne.size - 1)
      toID = last.toNode.id
    }

    def buildTopKPathsEarly(topk: Int, methodType: Int): ArrayBuffer[Path] = {
      var selected: Path = null
      selected = shortestOne
      if (Parameter.detail == true) System.out.println("0 : " + selected.toString)
      topks.append(selected)
      var itr = 0
      val paths = new PathCandidates(selected)
      while (!paths.enoughResults(topk)) {
        var tmp = ArrayBuffer[Path]()
        if (methodType == COMBINEYENEPS)
          tmp = selected.getNextPaths(topks, toID)
        tmp.foreach(paths.addOneCandidatePathWithoutTesting _)
        selected = paths.getCurrent
        if (selected != null) {
          topks.append(selected)
          itr += 1
        }
      }
      paths.outPutRestResult(topk, itr)
      topks
    }

    def buildTopKPathsNormal(topk: Int, methodType: Int): ArrayBuffer[Path] = {
      var selected: Path = null
      selected = shortestOne
      if (Parameter.detail == true) System.out.println("Path 0 : " + selected.toString)
      topks.append(selected)
      var itr = 0
      while (itr < topk) {
        var tmp = ArrayBuffer[Path]()
        if (methodType == TopKPaths.COMBINEYENEPS)
          tmp = selected.getNextPaths(topks, toID)

        for (oneCandidate <- tmp
             if !this.isConstainedIn(candidates, oneCandidate)
               && !this.isConstainedIn(topks, oneCandidate)) {
          candidates.append(oneCandidate)
        }

        if (candidates.isEmpty) return topks
        selected = candidates.head
        selected = candidates.find(_.totalDistance < selected.totalDistance).getOrElse(selected)
        candidates.remove(candidates.indexOf(selected))
        if (Parameter.detail)
          System.out.println("Path " + (itr + 1) + " : " + selected.toString)
        topks.append(selected)
        itr += 1
      }
      topks
    }

    private def isConstainedIn(paths: ArrayBuffer[Path], newPath: Path): Boolean =
      paths.find(_.isEqual(newPath)).nonEmpty
  }

  class ShortestPath {
    var queryNumber = 100
    var simpleTimes1 = 0L
    var simpleTimes2 = 0L
    var indexTimes = 0L
    var simpleDistance1 = 0
    var simpleDistance2 = 0
    var indexDistance = 0
    var source = 0
    var target = 9999

    def topKShortestPath(graph: Graph) {
      this.ourMemoryTopK(graph, source, target)
    }

    def ourMemoryTopK(dgraph: Graph, source: Int, target: Int) {
      var startTime1 = 0L
      var startTime2 = 0L
      var spt: ShortestPathTree = null
      var content: String = ""
      System.out.println("Begin our method-------------------")
      startTime1 = System.currentTimeMillis
      spt = new ShortestPathTree(dgraph, target, ShortestPathTree.IN)
      System.out.println("Constructing SPT...")
      spt.constructRevSPTInMem_Fib
      System.out.println("Making PrePostParentAnnotations...")
      spt.makePrePostParentAnnotation
      content = content + " Build First Shortest Path Tree=" + (System.currentTimeMillis - startTime1)
      val spttime = (System.currentTimeMillis - startTime1)
      System.out.println("Setting extra costs...")
      dgraph.setSideCost
      startTime2 = System.currentTimeMillis
      ShortestPathTreeSideCost.resetStaticForNextTime
      if (!Parameter.pruningNodes) {
        ShortestPathTreeSideCost.sideCostThreshold = Int.MaxValue
      }
      System.out.println("Building shortest paths...")
      val sf: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd")
      val date = new Date
      System.out.println(sf.format(date))
      var ii = 0
      var jj = 0

      {
        jj = 0
        while (jj <= 1) {
          {
            if (jj == 0) Parameter.earlyTerminate = false
            else Parameter.earlyTerminate = true

            {
              ii = 0
              while (ii < 3) {
                {
                  System.out.println("===========================================================")
                  if (ii == 0) {
                    Parameter.pruningNodes = false
                    if (Parameter.earlyTerminate == true) {
                      System.out.println("Testing KR: k-reduction strategy, without pruning optimizations")
                    }
                    else {
                      System.out.println("Testing NM: normal termination, without pruning optimizations")
                    }
                  }
                  if (ii == 1) {
                    Parameter.pruningNodes = true
                    ShortestPathTreeSideCost.EL = 0
                    if (Parameter.earlyTerminate == true) {
                      System.out.println("Testing KRE: k-reduction with Eager Strategy")
                    }
                    else {
                      System.out.println("Testing NME: normal termination with Eager Strategy")
                    }
                  }
                  if (ii == 2) {
                    Parameter.pruningNodes = true
                    ShortestPathTreeSideCost.EL = 1
                    if (Parameter.earlyTerminate == true) {
                      System.out.println("Testing KRL: k-reduction with Lazy Strategy")
                    }
                    else {
                      System.out.println("Testing NML: normal termination with Lazy Strategy")
                    }
                  }
                  System.gc
                  System.out.println("k=" + Parameter.topks + " :")
                  dgraph.buildTopKPaths(source, target, Parameter.topks, TopKPaths.COMBINEYENEPS)
                  content = content + "  Locate top k paths=" + (System.currentTimeMillis - startTime2)
                  content = content + "  total time cost =" + ((System.currentTimeMillis - startTime2) + spttime)
                  content = content + " \r\n The threshold : " + ShortestPathTreeSideCost.sideCostThreshold
                  content = content + "\r\n"
                  System.out.println(content)
                  startTime2 = System.currentTimeMillis
                  content = null
                  System.gc
                  content = ""
                  content = content + " Build First Shortest Path Tree=" + spttime
                  ShortestPathTreeSideCost.sideCostThreshold = Int.MaxValue
                  ShortestPathTreeSideCost.totalCandidates = 0
                  ShortestPathTreeSideCost.rtotalCandidates = 0
                  ShortestPathTreeSideCost.maxThreshold = 0
                  ShortestPathTreeSideCost.sideCostThreshold = Int.MaxValue
                }
                ({
                  ii += 1;
                  ii - 1
                })
              }
            }
          }
          ({
            jj += 1;
            jj - 1
          })
        }
      }
    }
  }

}

object Gao {
  def kShortestPath(lookup: Map[Int, List[(Int, Int)]], end: Int, start: Int = 0, k: Int = 10, terminateEarly: Boolean = false, pruneNodes: Boolean = false) = {
    val gao = new Gao
    val graph = new gao.Graph(lookup.size)
    for {(fromNode, edges) <- lookup
         (weight, toNode) <- edges}
      graph.nodes(fromNode).addEdge(graph.nodes(toNode), weight)

    gao.Parameter.earlyTerminate = terminateEarly
    gao.Parameter.pruningNodes = pruneNodes
    if (pruneNodes) gao.ShortestPathTreeSideCost.EL = 1
    else gao.ShortestPathTreeSideCost.sideCostThreshold = Int.MaxValue

    val spt = new gao.ShortestPathTree(graph, end, gao.ShortestPathTree.IN)
    spt.constructRevSPTInMem_Fib
    spt.makePrePostParentAnnotation
    graph.setSideCost
    gao.ShortestPathTreeSideCost.resetStaticForNextTime

    val topkps = graph.buildTopKPaths(start, end, k - 1 max 0, gao.TopKPaths.COMBINEYENEPS)
    topkps.map(sp => (sp.edges.map(e => e.fromNode.id) += sp.edges.last.toNode.id).toArray).toArray
  }
}