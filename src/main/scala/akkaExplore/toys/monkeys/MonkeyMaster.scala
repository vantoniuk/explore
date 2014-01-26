package akkaExplore.toys.monkeys

import akka.actor.{Props, ActorRef, Actor, ActorLogging}
import scala.annotation.tailrec
import akkaExplore.toys.monkeys.MonkeyMessage._
import akkaExplore.toys.monkeys.MonkeyMessage.DoTyping
import akkaExplore.toys.monkeys.MonkeyMessage.TypingGoal

class MonkeyMaster(config: MonkeyConfig) extends Actor with ActorLogging  {
  lazy val monkeys: Seq[ActorRef] = prepareMonkeys(config.numberOfTypists, config.symbolsToType)

  var messageQueue: Seq[(String, ActorRef)] = Seq.empty
  var results: Vector[String] = Vector.empty
  val tasksAtOneTime: Int = 1000 //Runtime.getRuntime.availableProcessors() * 10

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

    prepare(tasksAtOneTime, Nil)
  }

  def busy: Receive = {
    case TypingResult(result) => receiveResult(result)
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
        if(results.isEmpty) putMonkeysToWork(monkeys)
      }
    case TypingResult(result) => receiveResult(result)
  }

  def receiveResult(result: String) {
    results = results :+ result
    val resultsSoFar = results.length
    if(resultsSoFar == config.numberOfTypists) {
      log.info("got all the results, searching for best....")
      val (goal, recipient) = messageQueue.head
      recipient ! TypingResult(findBestResult(results, goal))
      messageQueue = messageQueue.tail
      results = Vector.empty
    } else {
      //log status
      if(resultsSoFar % tasksAtOneTime == 0) putMonkeysToWork(monkeys)
      if(resultsSoFar % (config.numberOfTypists / 100) == 0)
        log.info(s"collected $resultsSoFar results out of ${config.numberOfTypists}")
    }
  }

  private def findBestResult(results: Vector[String], goal: String): String = {
    val splittedGoal = goal.split(config.typingMachine.space).toList.distinct

    val resultsAbsolute = (results map { r =>
      splittedGoal intersect r.split(config.typingMachine.space).toList
    } sortBy (- _.length)).head mkString config.typingMachine.space.toString

    if(resultsAbsolute.nonEmpty) {
      resultsAbsolute
    }
    else {
      log.info("no absolute match")
      val splittedGoalSet = splittedGoal.toSet
      (results map { r => r.split(config.typingMachine.space).toList.filter( splittedGoalSet(_) )
      } sortBy (- _.length)).head mkString config.typingMachine.space.toString

    }
  }

  def putMonkeysToWork(availableMonkeys: Seq[ActorRef]) {
    availableMonkeys foreach (_ ! DoTyping(config.typingMachine.alphabet))
  }

}
