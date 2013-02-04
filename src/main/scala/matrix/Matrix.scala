package matrix
import collection.mutable.ArrayBuffer

/**
 * Matrix class
 */
trait Matrix[A] {
  type MatrixType = ArrayBuffer[ArrayBuffer[A]]

  def matrix: MatrixType

  /* Matrix tools */

  def update(in: MatrixType): Matrix[A]

  def createEmpty(inN: Int, inM: Int) = {
    val m = new MatrixType(inN)
    for (i <- 0 until inN) m += new ArrayBuffer[A](inM)
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

  protected def +| (x: A, y: A): A

  /**
   * subtraction of base elements of type A
   * @param x
   * @param y
   * @return x + y
   */

  protected def -| (x: A, y: A): A

  /**
   * Multiplication of base elements of type A
   * @param x
   * @param y
   * @return x + y
   */

  protected def *| (x: A, y: A): A

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
   * Function that applies function for corresponding elements and constructs a new matrix
   * @param that
   * @param f
   * @return
   */

  def map2(that: Matrix[A])(f: (A, A) => A): Option[Matrix[A]] = {
    if (n == that.n && m == that.m) {

      val mMapped = createEmpty(n, m)

      /*If dimension of matrices match it's possible to add matrices*/

      for (i <- 0 until n) {
        val (rowThis, rowThat) = (matrix(i), that.matrix(i))
        for (j <- 0 until m) {
          mMapped.matrix(i) += f(rowThis(j), rowThat(j))
        }
      }

      Some(mMapped)
    } else None
  }

  /**
   * Addition of matrices (requires the same dimensions)
   * @param that - Matrix to add to current
   * @return Matrix, result of addition
   */

  def + (that: Matrix[A]): Option[Matrix[A]] = map2(that)(+|)

  /**
   * Subtraction of matrices (requires the same dimensions)
   * @param that - Matrix to subtract from current
   * @return Matrix, result of sbtraction
   */

  def - (that: Matrix[A]): Option[Matrix[A]] = map2(that)(-|)

  /**
   * Get specific element from Matrix
   * @param row
   * @param col
   * @return
   */
  def apply(row: Int, col: Int) = get(row, col)
}
