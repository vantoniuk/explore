package akkaExplore.toys.monkeys

import akka.actor.{Props, ActorSystem}
import scala.concurrent.Future
import akkaExplore.toys.monkeys.MonkeyMessage.{TypingResult, TypingGoal}
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.ExecutionContext.Implicits._

/**
 * Object emulates the monkey typists work
 */
class MonkeysMain(config: MonkeyConfig) {
  private val system = ActorSystem("monkey-typers")

  private val monkeysMaster = system.actorOf(Props(new MonkeyMaster(config)))

  private implicit val timeout = Timeout(config.waitSeconds * 1000 toLong)

  def tryToType(text: String): Future[String] = {
    monkeysMaster ? TypingGoal(text) map {
      case TypingResult(result) => result
      case response => response.toString
    }
  }


  def finish() {
    system.shutdown()
  }

}
