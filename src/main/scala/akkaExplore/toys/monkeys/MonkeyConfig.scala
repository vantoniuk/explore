package akkaExplore.toys.monkeys

/**
 * The configuration of the monkey typist system
 */
case class MonkeyConfig(
                         tasks: Int,
                         workers: Int,
                         supervisors: Int,
                         symbolsToType: Int, 
                         capacity: Int, 
                         waitSeconds: Int, 
                         typingMachine: TypingMachine)

object MonkeyConfig {
  def apply(workersTotal: Int, supervisors: Int, workersForSupervisor: Int, symbolsToType: Int, capacity: Int, waitSeconds: Int): MonkeyConfig =
    MonkeyConfig(workersTotal, supervisors, workersForSupervisor, symbolsToType, capacity, waitSeconds, TypingMachine.simpleEnglishMachine)
}

case class TypingMachine(alphabet: Array[Char], space: Char)

object TypingMachine {
  val simpleEnglish = "abcde fghijk lmnopqr stuv wxyz "
  val space = ' '

  val simpleEnglishMachine: TypingMachine = TypingMachine(simpleEnglish.toCharArray, space)
}