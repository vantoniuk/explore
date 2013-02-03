package matrix

/**
 * Matrix class
 */
trait Matrix[A] {
  def matrix: Vector[Vector[A]]

  def update(in: Vector[Vector[A]]): Matrix[A]

  /**
   * N dimension of matrix
   * @return Int
   */
  def n: Int = matrix.size

  /**
   * M dimension of matrix
   * @return Int
   */
  def m: Int = if (matrix.isEmpty) 0 else matrix(0).size

  /*
  * Following will be element operation to provide polymorphism
  * */

  /**
   * Zero element of type A (e.g., 0 for Int)
   */
  val zeroA: A

  /**
   * Addition of base elements of type A
   * @param x
   * @param y
   * @return x + y
   */

  def +| (x: A, y: A): A

  /**
   * subtraction of base elements of type A
   * @param x
   * @param y
   * @return x + y
   */

  def -| (x: A, y: A): A

  /**
   * Multiplication of base elements of type A
   * @param x
   * @param y
   * @return x + y
   */

  def *| (x: A, y: A): A

  def get(row: Int, col: Int): A = matrix(row)(col)

  /*
  * Following will be Matrix operations
  * */

  /**
   * Multiplication of matrices
   * @param that - Matrix to multiply with current
   * @return Matrix, result of multiplication
   */

  def X (that: Matrix[A]): Matrix[A]

  /**
   * Addition of matrices (requires the same dimensions)
   * @param that - Matrix to add to current
   * @return Matrix, result of addition
   */

  def + (that: Matrix[A]): Option[Matrix[A]] = {
    if (n == that.n && m == that.m) {

      /*If dimension of matrices match it's possible to add matrices*/

      Some(this)
    } else None

  }

  /**
   * Get specific element from Matrix
   * @param row
   * @param col
   * @return
   */
  def apply(row: Int, col: Int) = get(row, col)
}

trait IterativeMultiplication[A] extends Matrix[A] {

  type MatrixA = Matrix[A]

  def X(that: MatrixA) = {
    if (that.n == m)
      multiply(that)
    else throw new IllegalArgumentException("Dimention of matrixes don't match!")
  }

  private def multiply(that: MatrixA): MatrixA = {

    def loop(row: Int, col: Int, acc: MatrixA): MatrixA = {
      if (col == that.m) loop(row + 1, 0, acc)
      else if (row == n) acc
      else {
        val el = (zeroA /: (0 until m))((newEl, index) => {
          //println("%s * %s, when row: %s, col: %s, index: %s".format(matrix(row)(index), that.matrix(index)(col), row, col, index))
           +|(*| (matrix(row)(index), that.matrix(index)(col)), newEl)
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
