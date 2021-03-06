import akkaExplore.toys.monkeys.{MonkeysMain, MonkeyConfig}
import scala.concurrent.ExecutionContext.Implicits._
import math.matrix.MatrixInt

object Solve extends App {

//  val v1 = Vector(
//    Vector(1,2),
//    Vector(2,1),
//    Vector(2,2)
//  )
//
//  val v2 = Vector(
//    Vector(3,2),
//    Vector(2,3)
//  )
//
//  val v3 = Vector(
//    Vector(5,5),
//    Vector(5,5)
//  )
//
//  val qM1 = Vector(
//    Vector(3,2, 3, 2),
//    Vector(2,3, 2, 3),
//    Vector(3,2, 3, 2),
//    Vector(2,3, 2, 3)
//  )
//
//  val qM2 = Vector(
//    Vector(1, 2, 3, 4),
//    Vector(1, 2, 1, 2),
//    Vector(2, 1, 2, 1),
//    Vector(1, 2, 1, 2)
//  )
//
//  val m1 = MatrixInt(v1)
//  val m2 = MatrixInt(v2)
// // val m3 = m1 X m2
//  val m4 = m1 + m1
//  val m5 = MatrixInt(v3) - m2
//  val m6 = MatrixInt(qM1) X MatrixInt(qM2)
//
//  println(MatrixInt(qM1))
//  println(MatrixInt(qM2))
//  println(m6)

  val monkeysMain = new MonkeysMain(MonkeyConfig(
    workersTotal = 100000,
    workersForSupervisor = 100,
    supervisors = 10,
    symbolsToType = 1000,
    capacity = 3,
    waitSeconds = 1000))

  monkeysMain.tryToType("test of the best monkey toy for people") map {
    r => println(r)
  } onComplete( _ => monkeysMain.finish())
}
