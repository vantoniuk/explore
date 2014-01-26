package akkaExplore.toys.monkeys

import akka.actor._
import akkaExplore.toys.monkeys.MonkeyMessage._
import scala.annotation.tailrec

/**
 * Supervisor that creates monkey and checks the result
 */
class MonkeySupervisor(numberOfTypists: Int, symbolsToType: Int, monkeysMaster: ActorRef) extends Actor with ActorLogging {
  val monkeys: List[ActorRef] = prepareMonkeys(numberOfTypists, symbolsToType)
  var workingMonkeys: Set[ActorRef] = Set.empty
  var textToCheck: List[String] = Nil
  var matchedResult: TypingMatchedResult = TypingMatchedResult.empty

  def receive: Receive = {
    case TextWithMachine(text, typingMachine) if workingMonkeys.isEmpty =>
      workingMonkeys = monkeys.toSet
      textToCheck = (text split typingMachine.space).toList.distinct
      monkeys foreach (_ ! DoTyping(typingMachine))

    case TypingResult(result, machine) =>
      val splittedResult = (result split machine.space).toList.distinct
      val matched = (splittedResult filter textToCheck.contains).mkString(machine.space.toString) :: Nil find(_.nonEmpty)
      val containedResults = textToCheck filter (part => splittedResult.exists(_.contains(part)))
      workingMonkeys -= sender
//      sender ! PoisonPill
      matchedResult = getBestResult(TypingMatchedResult(matched, containedResults), matchedResult)

      if(workingMonkeys.isEmpty) monkeysMaster ! matchedResult

    case _ => sender ! StillTyping

  }

  private def prepareMonkeys(monkeysToType: Int, symbolsToType: Int): List[ActorRef] = {
    @tailrec
    def prepare(left: Int, monkeys: List[ActorRef]): List[ActorRef] = {
      if(left == 0) {
        monkeys
      }
      else prepare(left - 1, context.actorOf(Props(new MonkeyTypist(symbolsToType))) :: monkeys)
    }

    prepare(monkeysToType, Nil)
  }
}
