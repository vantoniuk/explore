package math.matrix.mul

import math.matrix.Matrix

trait MatrixMultiplication[A] { Self: Matrix[A] =>

  type MatrixA = Matrix[A]

  def X (that: MatrixA) = {
    if (that.n == m)
      multiply(that)
    else throw new IllegalArgumentException("Dimention of matrixes don't match!")
  }

  protected def multiply(that: MatrixA): MatrixA

}
