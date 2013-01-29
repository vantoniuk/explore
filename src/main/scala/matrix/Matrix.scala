package matrix

/**
 * Matrix class
 */
trait Matrix {
  def matrix: Vector[Vector[Int]]

  def update(in: Vector[Vector[Int]]): Matrix

  /**
   * N dimension of matrix
   * @return Int
   */
  def n: Int

  /**
   * M dimension of matrix
   * @return Int
   */



  def m: Int

  def get(row: Int, col: Int): Int

  def X(that: Matrix): Matrix
}

trait IterativeIntMultiplication extends Matrix {

  def X(that: Matrix) = {
    if (that.n == m)
      multiply(that)
    else throw new IllegalArgumentException("Dimention of matrixes don't match!")
  }

  private def multiply(that: Matrix): Matrix = {

    def loop(row: Int, col: Int, acc: Matrix): Matrix = {
      if (col == that.m) loop(row + 1, 0, acc)
      else if (row == n) acc
      else {
        val el = (0 /: (0 until m))((newEl, index) => {
          //println("%s * %s, when row: %s, col: %s, index: %s".format(matrix(row)(index), that.matrix(index)(col), row, col, index))
          matrix(row)(index) * that.matrix(index)(col) + newEl
        })
        val updatedVector = if (acc.matrix.isEmpty)
          Vector(Vector(el))
        else if (acc.matrix.isDefinedAt(row))
          acc.matrix.updated(row, acc.matrix(row) :+ el)
        else acc.matrix :+ Vector(el)
        loop(row, col + 1, update(updatedVector))
      }
    }

    loop(0, 0, update(Vector.empty))
  }
}
