package akkaExplore.toys.monkeys

import akka.actor.ActorRef

object MonkeyMessage {

  case class TypingGoal(text: String)
  case class DoTyping(alphabet: Array[Char])
  case class TypingDone(result: String, recipient: ActorRef)
  case class TypingResult(result: String)
  case object CheckStatus
  case object Ok
  case object InQueue {
    override def toString: String = "Your request in queue, please wait."
  }
  case object Busy {
    override def toString: String = "Service is busy now!"
  }
  case object StillTyping

}
