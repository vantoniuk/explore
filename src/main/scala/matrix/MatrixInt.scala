package matrix

case class MatrixInt(matrix: Vector[Vector[Int]]) extends Matrix[Int] with IterativeMultiplication[Int] {

  /* Base methods */

  val zeroA = 0

  def +| (x: Int, y: Int): Int = x + y

  def -| (x: Int, y: Int): Int = x - y

  def *| (x: Int, y: Int): Int = x * y

  def update(in: Vector[Vector[Int]]) = new MatrixInt(in)

  override def toString: String = ("\nMatrix %s X %s:".format(n, m)) + ((matrix map {
    case row => row.mkString("\n | ", " | ", " | ")
  }) mkString)
}