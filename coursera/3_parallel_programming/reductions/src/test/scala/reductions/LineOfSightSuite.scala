package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("parLineOfSight should have same result as lineOfSight") {
    val input = Array(0f, 22f, 36f, 41f, 39f, 34f, 28f, 20f, 11f, 14f, 27f,
                      45f, 65f, 83f, 100f, 115f, 123f, 121f, 111f, 97f, 84f,
                      72f, 69f, 72f, 80f, 105f, 137f, 175f, 214f, 233f, 246f,
                      251f, 246f, 232f, 212f, 189f, 168f, 0f)
    val outputSeq = new Array[Float](input.length)
    val outputPar = new Array[Float](input.length)
    val threshold = 5
    lineOfSight(input, outputSeq)
    parLineOfSight(input, outputPar, threshold)
    assert(outputSeq.toList == outputPar.toList)
  }

}
