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

  override def toString: String = {
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
    val array = ArrayBuffer.fill[FibHeapNode[T]](arraySize)(null.asInstanceOf[FibHeapNode[T]])
    var numRoots = 0
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
      var y: FibHeapNode[T] = array(d)
      while (y != null) {
        if (x.key > y.key) {
          val temp: FibHeapNode[T] = y
          y = x
          x = temp
        }
        link(y, x)
        array.update(d, null)
        d += 1
        y = array(d)
      }

      array.update(d, x)
      x = next
      numRoots -= 1
    }

    minNode = null
    var i: Int = 0
    while (i < arraySize) {
      val y: FibHeapNode[T] = array(i)
      if (y != null) {
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
      i += 1;
    }
  }

  def cut(x: FibHeapNode[T], y: FibHeapNode[T]) {
    x.left.right = x.right
    x.right.left = x.left
    y.degree -= 1
    if (y.child eq x) {
      y.child = x.right
    }
    if (y.degree == 0) {
      y.child = null
    }
    x.left = minNode
    x.right = minNode.right
    minNode.right = x
    x.right.left = x
    x.parent = null
    x.mark = false
  }

  def link(y: FibHeapNode[T], x: FibHeapNode[T]) {
    y.left.right = y.right
    y.right.left = y.left
    y.parent = x
    if (x.child == null) {
      x.child = y
      y.right = y
      y.left = y
    }
    else {
      y.left = x.child
      y.right = x.child.right
      x.child.right = y
      y.right.left = y
    }
    x.degree += 1
    y.mark = false
  }
}

object FibHeap {
  val oneOverLogPhi: Double = 1.0 / Math.log((1.0 + Math.sqrt(5.0)) / 2.0)

  def union[T](h1: FibHeap[T], h2: FibHeap[T]): FibHeap[T] = {
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

}