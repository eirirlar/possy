package com.kodeworks.possy

import FibHeap._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack

class FibHeap[T] {
  var minNode: FibHeapNode[T] = null
  var nNodes = 0

  def isEmpty = minNode == null

  def clear {
    minNode = null
    nNodes = 0
  }

  def decreaseKey(x: FibHeapNode[T], k: Double) {
    if (k > x.key) {
      throw new IllegalArgumentException("decreaseKey() got larger key value")
    }
    x.key = k
    val y: FibHeapNode[T] = x.parent
    if ((y != null) && (x.key < y.key)) {
      cut(x, y)
      cascadingCut(y)
    }
    if (x.key < minNode.key) {
      minNode = x
    }
  }

  def delete(x: FibHeapNode[T]) {
    decreaseKey(x, Double.NegativeInfinity)
    removeMin
  }

  def insert(node: FibHeapNode[T], key: Double) {
    node.key = key
    if (minNode != null) {
      node.left = minNode
      node.right = minNode.right
      minNode.right = node
      node.right.left = node
      if (key < minNode.key) {
        minNode = node
      }
    }
    else {
      minNode = node
    }
    nNodes += 1
  }

  def min = minNode

  def removeMin: FibHeapNode[T] = {
    val z: FibHeapNode[T] = minNode
    if (z != null) {
      var numKids: Int = z.degree
      var x: FibHeapNode[T] = z.child
      var tempRight: FibHeapNode[T] = null
      while (numKids > 0) {
        tempRight = x.right
        x.left.right = x.right
        x.right.left = x.left
        x.left = minNode
        x.right = minNode.right
        minNode.right = x
        x.right.left = x
        x.parent = null
        x = tempRight
        numKids -= 1
      }
      z.left.right = z.right
      z.right.left = z.left
      if (z eq z.right) {
        minNode = null
      }
      else {
        minNode = z.right
        consolidate
      }
      nNodes -= 1
    }
    return z
  }

  def size = nNodes

  override def toString:String = {
    if (minNode == null)
      return "FibonacciHeap=[]"
    val stack: Stack[FibHeapNode[T]] = new Stack[FibHeapNode[T]]
    stack.push(minNode)
    val buf: StringBuffer = new StringBuffer(512)
    buf.append("FibonacciHeap=[")
    while (!stack.isEmpty) {
      var curr: FibHeapNode[T] = stack.pop
      buf.append(curr)
      buf.append(", ")
      if (curr.child != null) {
        stack.push(curr.child)
      }
      val start: FibHeapNode[T] = curr
      curr = curr.right
      while (curr ne start) {
        buf.append(curr)
        buf.append(", ")
        if (curr.child != null) {
          stack.push(curr.child)
        }
        curr = curr.right
      }
    }
    buf.append(']')
    return buf.toString
  }

  def cascadingCut(y: FibHeapNode[T]) {
    val z: FibHeapNode[T] = y.parent
    if (z != null) {
      if (!y.mark) {
        y.mark = true
      }
      else {
        cut(y, z)
        cascadingCut(z)
      }
    }
  }

  protected def consolidate {
    val arraySize: Int = (Math.floor(Math.log(nNodes) * oneOverLogPhi).toInt) + 1
    val array: Array[FibHeapNode[T]] = new ArrayBuffer[FibHeapNode[T]](arraySize) {
      var i: Int = 0
      while (i < arraySize) {
        {
          array += null.asInstanceOf[FibHeapNode[T]]
        }
        ({
          i += 1;
          i - 1
        })
      }
    }
    var numRoots: Int = 0
    var x: FibHeapNode[T] = minNode
    if (x != null) {
      numRoots += 1
      x = x.right
      while (x ne minNode) {
        numRoots += 1
        x = x.right
      }
    }
    while (numRoots > 0) {
      var d: Int = x.degree
      val next: FibHeapNode[T] = x.right
      while (true) {
        var y: FibHeapNode[T] = array.get(d)
        if (y == null) {
          break //todo: break is not supported
        }
        if (x.key > y.key) {
          val temp: FibHeapNode[T] = y
          y = x
          x = temp
        }
        link(y, x)
        array.set(d, null)
        d += 1
      }
      array.set(d, x)
      x = next
      numRoots -= 1
    }
    minNode = null {
      var i: Int = 0
      while (i < arraySize) {
        {
          val y: FibHeapNode[T] = array.get(i)
          if (y == null) {
            continue //todo: continue is not supported
          }
          if (minNode != null) {
            y.left.right = y.right
            y.right.left = y.left
            y.left = minNode
            y.right = minNode.right
            minNode.right = y
            y.right.left = y
            if (y.key < minNode.key) {
              minNode = y
            }
          }
          else {
            minNode = y
          }
        }
        ({
          i += 1;
          i - 1
        })
      }
    }
  }
}

object FibHeap {
  val oneOverLogPhi: Double = 1.0 / Math.log((1.0 + Math.sqrt(5.0)) / 2.0)
}

class FibHeapNode[T](
                      var data: T,
                      var key: Double
                      ) {
  var left: FibHeapNode[T] = this
  var right: FibHeapNode[T] = this
  var child: FibHeapNode[T] = null
  var parent: FibHeapNode[T] = null
  var mark = false
  var degree = 0

  def getKey = key

  def getData = data

  override def toString: String = {
    if (true) {
      return key.toString
    }
    else {
      val buf: StringBuffer = new StringBuffer
      buf.append("Node=[parent = ")
      if (parent != null) {
        buf.append(parent.key.toString)
      }
      else {
        buf.append("---")
      }
      buf.append(", key = ")
      buf.append(key.toString)
      buf.append(", degree = ")
      buf.append(Integer.toString(degree))
      buf.append(", right = ")
      if (right != null) {
        buf.append(right.key.toString)
      }
      else {
        buf.append("---")
      }
      buf.append(", left = ")
      if (left != null) {
        buf.append(left.key.toString)
      }
      else {
        buf.append("---")
      }
      buf.append(", child = ")
      if (child != null) {
        buf.append(child.key.toString)
      }
      else {
        buf.append("---")
      }
      buf.append(']')
      return buf.toString
    }
  }

  def union(h1: FibHeap[T], h2: FibHeap[T]): FibHeap[T] = {
    val h: FibHeap[T] = new FibHeap[T]
    if ((h1 != null) && (h2 != null)) {
      h.minNode = h1.minNode
      if (h.minNode != null) {
        if (h2.minNode != null) {
          h.minNode.right.left = h2.minNode.left
          h2.minNode.left.right = h.minNode.right
          h.minNode.right = h2.minNode
          h2.minNode.left = h.minNode
          if (h2.minNode.key < h1.minNode.key) {
            h.minNode = h2.minNode
          }
        }
      }
      else {
        h.minNode = h2.minNode
      }
      h.nNodes = h1.nNodes + h2.nNodes
    }
    return h
  }
}