package ParallelAnts

import scala.actors.Actor
import scala.actors.Actor._

case class Result(solution: MatrixInt, fitness: Double)

class ResultGatherer(startTime: Long, antCount: Int) extends Actor {
  def act() {
    var msgCount = 0

    while (msgCount < antCount) {
      receive {
        case Result(path, pathLength) =>
          println("==Gathered==")
          print(path)
          print(" : ")
          println(pathLength)
          msgCount += 1
      }
    }       

    val stopTime = System.currentTimeMillis();
    print("parallel runtime in millis = ")
    println(stopTime - startTime)
  } 
}