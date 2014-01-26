package akkaExplore.toys.monkeys

import akka.actor.{Props, ActorRef, Actor, ActorLogging}
import scala.annotation.tailrec
import akkaExplore.toys.monkeys.MonkeyMessage._
import akkaExplore.toys.monkeys.MonkeyMessage.TypingGoal

class MonkeyMaster(config: MonkeyConfig) extends Actor with ActorLogging  {
  lazy val supervisors: Seq[ActorRef] = prepareSupervisors()

  var messageQueue: Seq[(String, ActorRef)] = Seq.empty
  var tasksLeft = 0
  var currentResult = TypingMatchedResult.empty
  var workingSupervisors: Set[ActorRef] = Set.empty

  def reset() {
    tasksLeft = config.tasks
    currentResult = TypingMatchedResult.empty
  }

  log.info(s"Started Monkey Typist system for ${config.tasks} monkeys")

  private def prepareSupervisors(): List[ActorRef] = {
    @tailrec
    def prepare(left: Int, supervisors: List[ActorRef]): List[ActorRef] = {
      if(left == 0) {
        log.info("Supervisors are ready")
        supervisors
      }
      else prepare(
        left - 1,
        context.actorOf(Props(new MonkeySupervisor(config.workers, config.symbolsToType, self))) :: supervisors
      )
    }

    prepare(config.supervisors, Nil)
  }

  def busy: Receive = {
    case result :TypingMatchedResult => receiveResult(result, sender)
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
        if(tasksLeft <= 0) {
          log.info(s"sending ${config.tasks} tasks to ${config.supervisors} supervisors with ${config.workers} monkeys")
          reset()
          setTasks(supervisors)
        }
      }
    case result :TypingMatchedResult => receiveResult(result, sender)
  }

  def receiveResult(result: TypingMatchedResult, worker: ActorRef) {
    workingSupervisors -= worker
    currentResult = getBestResult(result, currentResult)
    if(tasksLeft <= 0 && workingSupervisors.isEmpty) {
      val (_, recipient) = messageQueue.head
      messageQueue = messageQueue.tail
      recipient ! currentResult
    } else {
      if(workingSupervisors.isEmpty) {
        log.info(s"$tasksLeft tasks left\nbest results: matched - ${currentResult.matched}, contained: ${currentResult.containedParts.mkString(" ")}")
        setTasks(supervisors)
      }
    }
  }

  def setTasks(supervisors: Seq[ActorRef]) {
    workingSupervisors = supervisors.take(tasksLeft).toSet
    tasksLeft -= config.supervisors * config.workers
    supervisors foreach (_ ! TextWithMachine(messageQueue.head._1, config.typingMachine))
  }

}
