package akkaExplore.toys

import akkaExplore.toys.monkeys.MonkeyMessage.TypingMatchedResult

package object monkeys {

  def getBestResult(result1: TypingMatchedResult, result2: TypingMatchedResult): TypingMatchedResult = {
    if(result1.matched.getOrElse("").length > result2.matched.getOrElse("").length ||
      result2.matched.isEmpty && result1.containedParts.length > result2.containedParts.length
    ) result1
    else result2
  }

}
