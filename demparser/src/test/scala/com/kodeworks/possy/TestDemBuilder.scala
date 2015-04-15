package com.kodeworks.possy

import org.junit.{Assert, Test}

class TestDemBuilder {
  @Test
  def testDemBuilder {
    val typeA = RecordTypeA("test", 0, 0, 0, 33, "", 10, 10, 4, 123f, 234f, 123f, 234f, 123f, 234f, 123f, 234f, 4f, 120f, 0f, 0, 10f, 10f, .1f, 5012, 1)
    val typeB1Head = RecordTypeBHead(1, 1, 5012, 1, 123f, 234f, 0f, 23f, 55f, Vector(23, 24, 25, 26, 27))
    val typeB1Tail1 = RecordTypeBTail(Vector(28, 29, 30, 31, 32, 33))
    val typeB1Tail2 = RecordTypeBTail(Vector(34, 35, 36, 37, 38, 39))
    val typeB2Head = RecordTypeBHead(1, 2, 5012, 1, 345f, 456f, 0f, 87f, 122f, Vector(87, 88, 89, 90, 91))
    val typeB2Tail1 = RecordTypeBTail(Vector(92, 93, 94, 95, 96))

    val builder = new DemBuilder()
    builder(typeA)
    builder(typeB1Head)
    builder(typeB1Tail1)
    builder(typeB1Tail2)
    builder(typeB2Head)
    builder(typeB2Tail1)
    var dem = builder.build
    Assert.assertEquals(96, dem.typeBs.last.elevations.last)
    println(dem)
  }

  @Test
  def testRecordMemory {
    val typeA = RecordTypeA("test", 0, 0, 0, 33, "", 10, 10, 4, 123f, 234f, 123f, 234f, 123f, 234f, 123f, 234f, 4f, 120f, 0f, 0, 10f, 10f, .1f, 1, 5041)
    val typeBHead = RecordTypeBHead(1, 1, 5041, 1, 123f, 234f, 0f, 23f, 55f, (0 to DemParser.typeBHeadMaxElevs).map(_.toShort).toVector)
    val typeBTail = RecordTypeBTail((0 to DemParser.typeBTailMaxElevs).map(_.toShort).toVector)
    val typeBTailLast = RecordTypeBTail((0 to 236).map(_.toShort).toVector)
    val builder = new DemBuilder()
    builder(typeA)
    var n = 0
    var last = System.currentTimeMillis
    for (i <- (0 to typeA.numberOfColumns)) {
      builder(typeBHead)
      if (n % 100 == 0) {
        println("column " + n + " in " + (System.currentTimeMillis() - last) + " millis, free: " + (Runtime.getRuntime.freeMemory() / 1024L / 1024L) + " MB")
        last = System.currentTimeMillis()
      }
      for (j <- (0 to 29)) {
        builder(typeBTail)
      }
      builder(typeBTailLast)
      n += 1
    }
  }

}
