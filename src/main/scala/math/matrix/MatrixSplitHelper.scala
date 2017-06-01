package math.matrix

class MatrixSplitHelper[A] {

  private def splitVerticalTuple2(inM: Matrix[A]): Option[(Matrix[A], Matrix[A])] = inM.splitVertical(2) map {
    case split2Arr => (split2Arr(0), split2Arr(1))
  }

  private def splitHorizontalTuple2(inM: Matrix[A]): Option[(Matrix[A], Matrix[A])] = inM.splitHorizontal(2) map {
    case split2Arr => (split2Arr(0), split2Arr(1))
  }

  private def splitEachInTuple(mTuple: (Matrix[A], Matrix[A])): Option[(Matrix[A], Matrix[A], Matrix[A], Matrix[A])] = {
    val (top, bot) = mTuple

    (splitHorizontalTuple2(top), splitHorizontalTuple2(bot)) match {
      case (Some((l1, r1)), Some((l2, r2))) => Some((l1, r1, l2, r2))
      case _ => None
    }

  }

  private def splitFromOptionTuple(tupleOpt: Option[(Matrix[A], Matrix[A])]) = tupleOpt flatMap splitEachInTuple

  /**
   * Split current Matrix into 4 pieces, combined as tuple4
   * @return tuple of matrices
   */

  def split4(inM: Matrix[A]): Option[(Matrix[A], Matrix[A], Matrix[A], Matrix[A])] =
    (splitVerticalTuple2 _ andThen splitFromOptionTuple ) (inM)

}