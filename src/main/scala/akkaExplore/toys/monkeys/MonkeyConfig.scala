package akkaExplore.toys.monkeys

/**
 * The configuration of the monkey typist system
 */
case class MonkeyConfig(numberOfTypists: Int, symbolsToType: Int, capacity: Int, waitSeconds: Int, typingMachine: TypingMachine)

object MonkeyConfig {
  def apply(numberOfTypists: Int, symbolsToType: Int, capacity: Int, waitSeconds: Int): MonkeyConfig =
    MonkeyConfig(numberOfTypists, symbolsToType, capacity, waitSeconds, TypingMachine.simpleEnglishMachine)
}

case class TypingMachine(alphabet: Array[Char], space: Char)

object TypingMachine {
  val simpleEnglish = "abcdefghijklmnopqrstuvwxyz "
  val space = ' '

  val simpleEnglishMachine: TypingMachine = TypingMachine(simpleEnglish.toCharArray, space)
}