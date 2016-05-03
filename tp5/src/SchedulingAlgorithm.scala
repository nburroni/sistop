import scala.collection.immutable.Queue

/**
  * Created by nico on 02/05/16.
  */
trait SchedulingAlgorithm {

  def addProcess(process: Process)
  def nextProcess: Run

}

case class Run(process: Process, time: Int)

class RoundRobinScheduling(processes: Queue[Process], quantum: Int) extends SchedulingAlgorithm {

  private var processQueue: Queue[Process] = processes

  override def addProcess(process: Process) = processQueue = processQueue enqueue process

  override def nextProcess: Run = processQueue dequeue match {
    case (process, queue) =>
      processQueue = queue
      val time = if (quantum <= process.remaining) quantum else process.remaining
      Run(process, time)
  }
}