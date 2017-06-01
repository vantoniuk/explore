package math.matrix.mul

import math.matrix.Matrix

trait StrassenMultiplication[A] extends RecursiveMultiplication[A] { Self: Matrix[A] =>
  override protected def multiply(that: Matrix[A]): Matrix[A] =
    (splitter.split4(this), splitter.split4(that)) match {
      case ( Some((mA, mB, mC, mD)), Some((mE, mF, mG, mH)) ) =>
        val mP1 = mA X (mF - mH).get
        val mP2 = (mA + mB).get X mH
        val mP3 = (mC + mD).get X mE
        val mP4 = mD X (mG - mE).get
        val mP5 = (mA + mD).get X (mE + mH).get
        val mP6 = (mB - mD).get X (mG + mH).get
        val mP7 = (mA - mC).get X (mE + mF).get


        val leftTop = ( ((mP5 + mP4).get - mP2).get + mP6 ).get
        val rightTop = (mP1 + mP2).get
        val leftBot = (mP3 + mP4).get
        val rightBot = ( ((mP1 + mP5).get - mP3).get - mP7 ).get

        val topMatrix = (leftTop appendRight rightTop).get
        val botMatrix = (leftBot appendRight rightBot).get

        (topMatrix appendBottom botMatrix).get

      case _ => throw new IllegalStateException("Matrices can't be multiplied")
    }
}
