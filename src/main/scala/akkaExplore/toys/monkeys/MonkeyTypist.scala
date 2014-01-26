package akkaExplore.toys.monkeys

import akka.actor.{ActorRef, ActorLogging, Actor}
import MonkeyMessage._
import scala.annotation.tailrec

/**
 * The MonkeyTypist Actor
 */
class MonkeyTypist(numberOfSymbols: Int) extends Actor with ActorLogging {

  def receive = {
    case DoTyping(alphabet) =>
      context.become(stillTyping)
      doTyping(numberOfSymbols, alphabet, sender)
  }

  def stillTyping: Receive = {
    case TypingDone(result, recipient) =>
      context.unbecome()
      recipient ! TypingResult(result)
    case _ => sender ! StillTyping
  }

  private def doTyping(symbolsToType: Int, alphabet: Array[Char], recipient: ActorRef) {
    @tailrec
    def collect(symbolsLeft: Int, collectedString: String): String = {
      if(symbolsLeft == 0) collectedString
      else {
        val ind = scala.util.Random.nextInt(alphabet.length)
        collect(symbolsLeft - 1, collectedString + alphabet(ind))
      }
    }

    val result = collect(symbolsToType, "")

    self ! TypingDone(result, recipient)
  }
}
