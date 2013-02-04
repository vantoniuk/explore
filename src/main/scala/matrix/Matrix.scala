package matrix
import collection.mutable.ArrayBuffer

/**
 * Matrix class
 */
trait Matrix[A] {
  def matrix: ArrayBuffer[ArrayBuffer[A]]

  def update(in: ArrayBuffer[ArrayBuffer[A]]): Matrix[A]

  def createEmpty(inN: Int, inM: Int) = {
    val m = new ArrayBuffer[ArrayBuffer[A]](inN)
    for (i <- 0 until inM) m += new ArrayBuffer[A](inM)
    update(m)
  }

  def addRow() {
    matrix += new ArrayBuffer[A](m)
  }

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
