import matrix.MatrixInt

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
  val v3 = Vector(
    Vector(5,5),
    Vector(5,5)
  )

  val m1 = MatrixInt(v1)
  val m2 = MatrixInt(v2)
  val m3 = m1 X m2
  val m4 = m1 + m1
  val m5 = MatrixInt(v3) - m2

  println(m1)
  println(m2)
  println(m3)
  println(m4)
  println(m5)
}
