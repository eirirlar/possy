package com.kodeworks.possy

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Gao {

  class Edge(
              var fromNode: Node,
              var toNode: Node,
              var distance: Int = 0
              ) {
    var sideCost = 0

    //TODO override equals instead
    def equal(edge1: Edge): Boolean =
      edge1.fromNode.id == fromNode.id && edge1.toNode.id == toNode.id

    def reverseEdge: Edge =
      new Edge(toNode, fromNode, distance)
  }

  class Node(val id: Int) {
    val outEdgesInGraph = ArrayBuffer[Edge]()
    val inEdgesInGraph = ArrayBuffer[Edge]()
    val edgesInSPT = ArrayBuffer[Edge]()
    var fibNode: FibHeapNode[Node] = null
    var preEdge: Edge = null
    var preEdgeSideCost: Edge = null
    var cost = Int.MaxValue
    var pre = 0
    var post = 0
    var parent = 0
    var sideCost = 0
    var minSumSideCost = Int.MaxValue
    var treeLevel = 0
    var inComingEdges = 0

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
      val newEdge: Edge = new Edge(this, toNode1, cost)
      addOutEdgeIntoGraph(newEdge)
      toNode1.addInEdgeIntoGraph(newEdge)
    }

    def addChildNode(childNode: Node) {
      val edge1: Edge = new Edge(this, childNode)
      addOutEdgeIntoGraph(edge1)
      childNode.addInEdgeIntoGraph(edge1)
    }

    def removeEdgeInSPT(edge1: Edge) {
      edgesInSPT.remove(edgesInSPT.indexOf(edge1))
    }

    def getToNodes: ArrayBuffer[Int] = {
      outEdgesInGraph.map(_.toNode.id)
    }

    def getPreNodeID: Int = {
      if (preEdge.fromNode eq this) preEdge.toNode.id
      else preEdge.fromNode.id
    }

    def getEdgeFromToNodeInSPT(toNodeID: Int): Edge =
      edgesInSPT.find(_.toNode.id == toNodeID).getOrElse(null)

    def annotateNode(pre: Int, parent: Int): Int = {
      this.parent = parent
      var currentPre: Int = pre
      this.pre = currentPre
      edgesInSPT.foreach(e =>
        currentPre = e.toNode.annotateNode(currentPre, id)
      )
      currentPre += 1
      post = currentPre
      currentPre
    }

    def setSideCost {
      outEdgesInGraph.foreach(e => e.sideCost = e.distance - cost + e.toNode.cost)
    }
  }

  class Path(val dgraph: Graph) {
    val edges = new ArrayBuffer[Edge]
    var totalDistance: Int = 0
    var sourceNode: Node = null
    var cnodeSideCost: Int = 0

    def addEdgeIntoPath(cedge: Edge) {
      edges.append(cedge)
      totalDistance += cedge.distance
    }

    def addEdgeFirst(cedge: Edge) {
      edges.prepend(cedge)
      totalDistance += cedge.distance
    }

    def isEqual(second: Path): Boolean = {
      if (this.edges.size != second.edges.size || second.totalDistance != totalDistance) return false
      edges.zip(second.edges).forall(es => es._1.fromNode.id == es._2.fromNode.id
        && es._1.toNode.id == es._2.toNode.id
        && es._1.distance == es._2.distance)
    }

    def getNextPaths(topks: ArrayBuffer[Path], toID: Int): ArrayBuffer[Path] = {
      val nexts = ArrayBuffer[Path]()
      val nodes = edges.map(_.fromNode)
      if (edges.nonEmpty)
        nodes.append(edges.last.toNode)
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
        i += 1
      }
      nexts
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
            && j < edges.size) {
            cedge1 = cpath.edges(j)
            cedge2 = edges(j)
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
        import scala.util.control.Breaks._
        breakable {
          while (j < cpath.edges.size && j < idx) {
            if (!cpath.edges(j).equal(edges(j))) break
            else j += 1
          }
        }
        if (j == idx
          && cpath.edges.size > idx
          && !nextEdges.contains(cpath.edges(idx)))
          nextEdges.append(cpath.edges(idx))
      }
      nextEdges
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

  class Graph(size: Int) {
    val nodes = ArrayBuffer.tabulate[Node](size)(new Node(_))

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
        cedge = new Edge(tmp, tmp.preEdge.toNode, tmp.preEdge.distance)
        tmp = nodes(tmp.parent)
        cedge.toNode = tmp
        spath.addEdgeIntoPath(cedge)
      }
      spath
    }

    def buildTopKPaths(fromNode: Int, toNode: Int, topk: Int): ArrayBuffer[Path] = {
      var spath: Path = getShortestPath(fromNode, toNode)
      spath.sourceNode = nodes(fromNode)
      var paths = new TopKPaths(this, spath)
      if (Parameter.earlyTerminate)
        paths.buildTopKPathsEarly(topk)
      else
        paths.buildTopKPathsNormal(topk)
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
    addOneCandidatePathWithoutTesting(shortest)

    def enough(k: Int, cur_num: Int, cur_len: Int, topks: ArrayBuffer[Path]): Boolean = {
      var same_len1 = 0
      var same_len2 = 0
      for (p <- topks
           if p.totalDistance == cur_len)
        same_len1 += 1

      for (ps <- candidates;
           p <- ps
           if p.totalDistance == cur_len) {
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
        import scala.util.control.Breaks._
        breakable {
          while (currentIdx < candidates.size) {
            currentResults = candidates(currentIdx)
            if (currentResults.size != 0)
              break
            else
              currentIdx += 1
          }
        }
        currentPos = 0
      }
      if (currentIdx == candidates.size) return null
      currentResults = candidates(currentIdx)
      val onePath: Path = currentResults(currentPos)
      currentPos += 1
      exactResults += 1
      idx += 1
      onePath
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
      if (currentIdx == candidates.size) return true
      val maxLen: Int = (((currentIdx + shortestDistance) * PathCandidates.app).toInt) - shortestDistance
      var sunResults: Int = candidates(currentIdx).size - currentPos
      var i: Int = currentIdx + 1
      while (i < maxLen && i < candidates.size) {
        sunResults = sunResults + candidates(i).size
        i += 1
      }
      ((exactResults + sunResults) > k)
    }
  }

  object ShortestPathTree {
    var OUT: Int = 1
    var IN: Int = 0
  }

  class ShortestPathTree(var dgraph: Graph, var rootID: Int, var direction: Int = 1) {
    val activeNodesList: ArrayBuffer[Node] = ArrayBuffer[Node]()
    val nodesFinished = mutable.Set[Node]()
    val fibHeap: FibHeap[Node] = new FibHeap[Node]
    val fibnodesHash = mutable.Map[AnyRef, AnyRef]()
    val leafNodesList = ArrayBuffer[Node]()
    var costs = ArrayBuffer[Int]()
    var mergeNode: Node = null
    var benefit = 0
    var maxCost = 0
    var totalNodes = 0
    var rootNode: Node = dgraph.nodes(rootID)
    rootNode.cost = 0

    def constructRevSPTInMem_Fib {
      var cnode: Node = null
      cnode = rootNode
      nodesFinished.add(cnode)
      import scala.util.control.Breaks._
      breakable {
        while (cnode != null) {
          extendInNodesInMemory_Fib(cnode)
          var n: FibHeapNode[Node] = null
          n = fibHeap.removeMin
          if (n == null) break
          else {
            cnode = n.getData
            cnode.fibNode = null
            nodesFinished.add(cnode)
          }
        }
      }
      initalCost(dgraph.nodes.size)
      visitTree
    }

    def extendInNodesInMemory_Fib(cnode: Node) {
      var fromID = 0
      var nextCost = 1
      var fromNode: Node = null
      var nextEdge: Edge = null
      var rs = dgraph.nodes(cnode.id).inEdgesInGraph
      for (edge <- rs) {
        fromID = edge.fromNode.id
        nextCost = edge.distance
        fromNode = dgraph.nodes(fromID)
        if (fromNode == null || !nodesFinished.contains(fromNode)) {
          if (fromNode.fibNode == null) {
            fromNode = dgraph.nodes(fromID)
            fromNode.cost = cnode.cost + nextCost
            nextEdge = new Edge(cnode, fromNode, nextCost)
            fromNode.preEdge = nextEdge
            cnode.addEdgeIntoSPT(nextEdge)
            val n: FibHeapNode[Node] = new FibHeapNode[Node](fromNode, fromNode.cost)
            fromNode.fibNode = n
            fibHeap.insert(n, n.getKey)
          }
          else if (fromNode.cost > cnode.cost + nextCost) {
            val pnodeID: Int = fromNode.getPreNodeID
            val pnode: Node = dgraph.nodes(pnodeID)
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
      var rs = dgraph.nodes(cnode.id).inEdgesInGraph
      for (edge <- rs) {
        fromID = edge.fromNode.id
        nextCost = edge.distance
        fromNode = dgraph.nodes(fromID)
        if (fromNode == null || nodesFinished.contains(fromNode)) {
          if (!activeNodesList.contains(fromNode)) {
            fromNode = dgraph.nodes(fromID)
            fromNode.cost = cnode.cost + nextCost
            nextEdge = new Edge(cnode, fromNode, nextCost)
            fromNode.preEdge = nextEdge
            cnode.addEdgeIntoSPT(nextEdge)
            activeNodesList.append(fromNode)
          }
          else if (fromNode.cost > cnode.cost + nextCost) {
            val pnodeID: Int = fromNode.getPreNodeID
            val pnode: Node = dgraph.nodes(pnodeID)
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
      var cnt = 0
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
          cnt += 1
        }
      }
      cnt
    }

    def makePrePostParentAnnotation {
      rootNode.annotateNode(0, -1)
    }
  }

  object Parameter {
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

    val sourceNode = dgraph.nodes(sourceID)
    val targetNode = dgraph.nodes(targetID)
    val activeNodesList = ArrayBuffer[Node]()
    val nodesFinished = ArrayBuffer[Node]()
    val removedNodes = ArrayBuffer[Node]()
    val leafNodesList = ArrayBuffer[Node]()
    var fibHeap: FibHeap[Node] = new FibHeap[Node]
    var initValue = 0

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

    def extendNodesInMemory_Fib(cnode: Node) {
      var toID = 0
      var nextCost = 1
      var toNode: Node = null
      val nextEdge: Edge = null
      var rs: ArrayBuffer[Edge] = dgraph.nodes(cnode.id).outEdgesInGraph
      for (edge <- rs
           if !isRemovedNextEdge(edge)) {
        toID = edge.toNode.id
        nextCost = edge.sideCost
        toNode = dgraph.nodes(toID)
        if (isValidateCandidate(toNode)) {
          if (toNode != null && nodesFinished.contains(toNode)) {
            if (toNode.treeLevel > cnode.treeLevel)
              toNode.inComingEdges += 1
          } else {
            if (toNode.fibNode == null) {
              toNode = dgraph.nodes(toID)
              toNode.preEdgeSideCost = edge
              toNode.sideCost = cnode.sideCost + nextCost
              if (toNode.sideCost < 0)
                toNode.sideCost = Int.MaxValue
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

      import scala.util.control.Breaks._
      breakable {
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
            return next
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
                break
              }
              if (EL == 0) {
                if (totalCandidates >= Parameter.topks) {
                  break
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
            else break
          }
        }
      }

      if (this.isTerminate(cnode) && next == null) {
        next = this.generatePath(cnode)
        if (next != null) {
          next.cnodeSideCost = cnode.sideCost
        }
      }
      while (!fibHeap.isEmpty) {
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
      if (cnode == null || (sourceNode eq cnode)) return null
      val spath: Path = new Path(dgraph)
      //TODO source of duplicate first entry on second, third, fourth etc shortest path?
      selected.edges.takeWhile(_.fromNode ne sourceNode).foreach(spath.addEdgeIntoPath _)
      spath.sourceNode = sourceNode
      val second: Path = new Path(dgraph)
      var tmp: Edge = cnode.preEdgeSideCost
      while (tmp.fromNode ne sourceNode) {
        second.addEdgeFirst(tmp)
        tmp = tmp.fromNode.preEdgeSideCost
      }
      spath.addEdgeIntoPath(tmp)
      spath.edges.foreach(spath.addEdgeIntoPath _)
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
//        System.out.println("current wrong path is " + spath.toString)
      }
      spath
    }

    private def isRemovedNextEdge(cedge: Edge): Boolean =
      removedEdges.find(_.equal(cedge)).nonEmpty

    def extendNodesInMemory(cnode: Node) {
      var toID = 0
      var nextCost = 1
      var toNode: Node = null
      val nextEdge: Edge = null
      var rs: ArrayBuffer[Edge] = null
      rs = dgraph.nodes(cnode.id).outEdgesInGraph

      for (edge <- rs) {
        if (!isRemovedNextEdge(edge)) {
          toID = edge.toNode.id
          nextCost = edge.sideCost
          toNode = dgraph.nodes(toID)
          if (!this.isValidateCandidate(toNode)) {
            if (toNode != null && nodesFinished.contains(toNode)) {
              if (toNode.treeLevel > cnode.treeLevel)
                toNode.inComingEdges += 1
            } else {
              if (!activeNodesList.contains(toNode)) {
                toNode = dgraph.nodes(toID)
                toNode.preEdgeSideCost = edge
                toNode.sideCost = cnode.sideCost + nextCost
                if (toNode.sideCost < 0)
                  toNode.sideCost = Int.MaxValue
                toNode.treeLevel = cnode.treeLevel + 1
                toNode.inComingEdges = 1
                if (toNode.sideCost <= sideCostThreshold)
                  activeNodesList.append(toNode)
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
        if (before.pre < cnode.pre && before.post > cnode.post)
          return 0
        tmp = before.preEdgeSideCost
        before = tmp.fromNode
      }
      1
    }
  }

  object ShortestPathTreeSideCost {
    var sideCostThreshold = Int.MaxValue
    var maxThreshold = 0
    var totalCandidates = 0
    var rtotalCandidates = 0
    var EL = 0
    var searchedNodes = 0
  }

  class TopKPaths(
                   val dgraph: Graph,
                   val shortestOne: Path) {
    val topks = ArrayBuffer[Path]()
    val candidates = ArrayBuffer[Path]()
    var toID = 0

    if (shortestOne.edges.nonEmpty) {
      toID = shortestOne.edges(shortestOne.edges.size - 1).toNode.id
    }

    def buildTopKPathsEarly(topk: Int): ArrayBuffer[Path] = {
      var selected = shortestOne
      topks.append(selected)
      var itr = 0
      val paths = new PathCandidates(selected)
      while (!paths.enoughResults(topk)) {
        var tmp = ArrayBuffer[Path]()
        tmp = selected.getNextPaths(topks, toID)
        tmp.foreach(paths.addOneCandidatePathWithoutTesting _)
        selected = paths.getCurrent
        if (selected != null) {
          topks.append(selected)
          itr += 1
        }
      }
      topks
    }

    def buildTopKPathsNormal(topk: Int): ArrayBuffer[Path] = {
      var selected = shortestOne
      topks.append(selected)
      var itr = 0
      while (itr < topk) {
        for (oneCandidate <- selected.getNextPaths(topks, toID)
             if !this.isConstainedIn(candidates, oneCandidate)
               && !this.isConstainedIn(topks, oneCandidate))
          candidates.append(oneCandidate)

        if (candidates.isEmpty) return topks
        selected = candidates.head
        selected = candidates.find(_.totalDistance < selected.totalDistance).getOrElse(selected)
        candidates.remove(candidates.indexOf(selected))
        topks.append(selected)
        itr += 1
      }
      topks
    }

    private def isConstainedIn(paths: ArrayBuffer[Path], newPath: Path): Boolean =
      paths.find(_.isEqual(newPath)).nonEmpty
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

    val topkps = graph.buildTopKPaths(start, end, k - 1 max 0)
    topkps.map(sp => (sp.edges.map(e => e.fromNode.id) += sp.edges.last.toNode.id).toArray).toArray
  }
}