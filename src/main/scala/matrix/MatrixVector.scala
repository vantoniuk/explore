package matrix

trait MatrixVector extends Matrix {
  def matrix: Vector[Vector[Int]]

  def n = matrix.size

  def m = if (matrix.isEmpty) 0 else matrix(0).size

  def get(row: Int, col: Int) = matrix(row)(col)

  def apply(row: Int, col: Int) = get(row, col)
}

case class MatrixIntVector(matrix: Vector[Vector[Int]]) extends MatrixVector with IterativeIntMultiplication {

  def update(in: Vector[Vector[Int]]) = new MatrixIntVector(in)

  override def toString: String = ("\nMatrix %s X %s:".format(n, m)) + ((matrix map {
    case row => row.mkString("\n | ", " | ", " | ")
  }) mkString)
}