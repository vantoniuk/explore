package matrix.mul

import matrix.Matrix

trait IterativeMultiplication[A] extends Matrix[A] {

  type MatrixA = Matrix[A]

  def X(that: MatrixA) = {
    if (that.n == m)
      multiply(that)
    else throw new IllegalArgumentException("Dimention of matrixes don't match!")
  }

  private def multiply(that: MatrixA): MatrixA = {

    val acc: MatrixA = createEmpty(n, that.m)

    def loop(row: Int, col: Int): MatrixA = {
      if (col == that.m) loop(row + 1, 0)
      else if (row == n) acc
      else {
        val el = (zeroA /: (0 until m))((newEl, index) => {
          //println("%s * %s, when row: %s, col: %s, index: %s".format(matrix(row)(index), that.matrix(index)(col), row, col, index))
           +|(*| (matrix(row)(index), that.matrix(index)(col)), newEl)
        })
        if (!acc.matrix.isDefinedAt(row))
          acc.addRow()
        acc.matrix(row) += el


        loop(row, col + 1)
      }
    }

    loop(0, 0)
  }
}
