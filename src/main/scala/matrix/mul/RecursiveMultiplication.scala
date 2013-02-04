package matrix.mul

import matrix.{MatrixSplitHelper, Matrix}

trait RecursiveMultiplication[A] extends MatrixMultiplication[A]{ Self: Matrix[A] =>

  protected val splitter = new MatrixSplitHelper[A]

  protected def isPow2(in: Int): Boolean = in == 2 || in % 2 == 0 && isPow2(in / 2)

  protected def canMultiply_?(inM: Matrix[A]): Boolean = m == n && (n == 1 || isPow2(n))

  protected def equalLength(inM1: Matrix[A], inM2: Matrix[A]): Boolean = inM1.n == inM2.n

  override def X (that: MatrixA) = {

    if (equalLength(this, that) && canMultiply_?(this) && canMultiply_?(that) ) {
      if (n == 1) {
        val resM = createEmpty(1,1)
        resM.matrix(0) += *|(get(0,0), that.get(0,0))
        resM
      } else multiply(that)
    }
    else throw new IllegalArgumentException("Dimention of matrixes don't match!")
  }

  protected def multiply(that: Matrix[A]): Matrix[A] =
    (splitter.split4(this), splitter.split4(that)) match {
      case ( Some((mA, mB, mC, mD)), Some((mE, mF, mG, mH)) ) =>
        val leftTop = ((mA X mE) + (mB X mG)).get
        val rightTop = ((mA X mF) + (mB X mH)).get
        val leftBot = ((mC X mE) + (mD X mG)).get
        val rightBot = ((mC X mF) + (mD X mH)).get

        val topMatrix = (leftTop appendRight rightTop).get
        val botMatrix = (leftBot appendRight rightBot).get

        (topMatrix appendBottom botMatrix).get

      case _ => throw new IllegalStateException("Matrices can't be multiplied")
    }
}
