package akkaExplore.toys.monkeys

import akka.actor.{ActorRef, ActorLogging, Actor}
import MonkeyMessage._
import scala.annotation.tailrec

/**
 * The MonkeyTypist Actor
 */
class MonkeyTypist(numberOfSymbols: Int) extends Actor with ActorLogging {

  def receive = {
    case DoTyping(machine) =>
      context.become(stillTyping)
      doTyping(numberOfSymbols, machine, sender)
  }

  def stillTyping: Receive = {
    case TypingDone(result, typingMachine, recipient) =>
      context.unbecome()
      recipient ! TypingResult(result, typingMachine)
    case _ => sender ! StillTyping
  }

  private def doTyping(symbolsToType: Int, machine: TypingMachine, recipient: ActorRef) {
    @tailrec
    def collect(symbolsLeft: Int, collectedString: String): String = {
      if(symbolsLeft == 0) collectedString
      else {
        val ind = scala.util.Random.nextInt(machine.alphabet.length)
        collect(symbolsLeft - 1, collectedString + machine.alphabet(ind))
      }
    }

    val result = collect(symbolsToType, "")

    self ! TypingDone(result, machine, recipient)
  }
}
