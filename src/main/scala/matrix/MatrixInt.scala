package matrix

import mul.RecursiveMultiplication
import collection.mutable.ArrayBuffer

object MatrixInt {

  private implicit def convertToMutable(in: IndexedSeq[IndexedSeq[Int]]): ArrayBuffer[ArrayBuffer[Int]] = {
    val result = new ArrayBuffer[ArrayBuffer[Int]](in.size)
    for (i <- 0 until in.size) {
      result += new ArrayBuffer(in(i).size)
      result(i) insertAll (0, in(i))
    }
    result
  }

  def apply(in: IndexedSeq[IndexedSeq[Int]]) = new MatrixInt(in)

}

class MatrixInt(val matrix: ArrayBuffer[ArrayBuffer[Int]]) extends Matrix[Int] with RecursiveMultiplication[Int] {

  /* Base methods */

  val zeroA = 0

  def +| (x: Int, y: Int): Int = x + y

  def -| (x: Int, y: Int): Int = x - y

  def *| (x: Int, y: Int): Int = x * y

  override def update(in: ArrayBuffer[ArrayBuffer[Int]]): Matrix[Int] = new MatrixInt(in)

  override def toString: String = ("\nMatrix %s X %s:".format(n, m)) + ((matrix map {
    case row => row.mkString("\n | ", " | ", " | ")
  }) mkString)
}