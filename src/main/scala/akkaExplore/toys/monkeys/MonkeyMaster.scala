package akkaExplore.toys.monkeys

import akka.actor.{Props, ActorRef, Actor, ActorLogging}
import scala.annotation.tailrec
import akkaExplore.toys.monkeys.MonkeyMessage._
import akkaExplore.toys.monkeys.MonkeyMessage.DoTyping
import akkaExplore.toys.monkeys.MonkeyMessage.TypingGoal

class MonkeyMaster(config: MonkeyConfig) extends Actor with ActorLogging  {
  lazy val monkeys: Seq[ActorRef] = prepareMonkeys(config.numberOfTypists, config.symbolsToType)
  var messageQueue: Seq[(String, ActorRef)] = Seq.empty
  var results: List[String] = Nil

  log.info(s"Started Monkey Typist system for ${config.numberOfTypists} monkeys")

  private def prepareMonkeys(monkeysToType: Int, symbolsToType: Int): List[ActorRef] = {
    @tailrec
    def prepare(left: Int, monkeys: List[ActorRef]): List[ActorRef] = {
      if(left == 0) {
        log.info("Monkeys are ready")
        monkeys
      }
      else prepare(left - 1, context.actorOf(Props(new MonkeyTypist(symbolsToType))) :: monkeys)
    }

    prepare(monkeysToType, Nil)
  }

  def busy: Receive = {
    case TypingResult(result) =>  receiveResult(result)
    case _ =>
      log.info(s"current capacity ${messageQueue.size}")
      sender ! Busy
  }

  def receive: Receive = {
    case TypingGoal(text) =>
      log.info(s"Received new typing goal:\n ---> $text")
      if(messageQueue.contains(text)) {
        sender ! InQueue
      } else {
        //TODO make it fully concurrent
        messageQueue = messageQueue :+ (text -> sender)
        //if the queue is too large, become busy
        if(messageQueue.size >= config.capacity) context.become(busy)
        log.info(s"sending task to ${config.numberOfTypists} monkeys")
        if(results.isEmpty) monkeys foreach (_ ! DoTyping(config.typingMachine.alphabet))
      }
    case TypingResult(result) =>  receiveResult(result)
  }

  def receiveResult(result: String) {
    results = result :: results
    val resultsSoFar = results.length
    if(resultsSoFar == config.numberOfTypists) {
      log.info("got all the results")
      val (goal, recipient) = messageQueue.head
      recipient ! TypingResult(findBestResult(results, goal))
      messageQueue = messageQueue.tail
      results = Nil
    } else {
      //log status
      if(resultsSoFar % (config.numberOfTypists / 10) == 0)
        log.info(s"collected $resultsSoFar results out of ${config.numberOfTypists}")
    }
  }

  private def findBestResult(results: List[String], goal: String): String = {
    val splittedGoal = goal.split(config.typingMachine.space).toList.distinct

    (results map { r =>
      splittedGoal intersect r.split(config.typingMachine.space).toList
    } sortBy (- _.length)).head mkString config.typingMachine.space.toString
  }

}
