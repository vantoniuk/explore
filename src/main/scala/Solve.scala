import matrix.MatrixIntVector

object Solve extends App {
  val v1 = Vector(
    Vector(1,2),
    Vector(2,1),
    Vector(2,2)
  )
  val v2 = Vector(
    Vector(3,2),
    Vector(2,3)
  )

  val m1 = MatrixIntVector(v1)
  val m2 = MatrixIntVector(v2)
  val m3 = m1 X m2


  println(m1)
  println(m2)
  println(m3)
}
