package akkaExplore.toys.monkeys

import akka.actor.ActorRef

object MonkeyMessage {

  case class TypingGoal(text: String)
  case class TextWithMachine(text: String, machine: TypingMachine)
  case class DoTyping(machine: TypingMachine)
  case class TypingDone(result: String, machine: TypingMachine, recipient: ActorRef)
  case class TypingResult(result: String, machine: TypingMachine)
  case class TypingMatchedResult(matched: Option[String], containedParts: List[String])
  object TypingMatchedResult {
    val empty = TypingMatchedResult(None, Nil)
  }

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
