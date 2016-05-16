import scala.collection.immutable.Queue

/**
  * Created by nico on 02/05/16.
  */
trait SchedulingAlgorithm {

  def addProcess(process: Process)
  def addProcesses(processes: List[Process])
  def nextProcess: Run
  def nonEmpty: Boolean
  def processes: List[Process]

}

case class Run(process: Process, time: Int)

case class RoundRobinScheduling(quantum: Int) extends SchedulingAlgorithm {

  private var processQueue = Queue[Process]()

  override def addProcess(process: Process) = processQueue = processQueue enqueue process
  override def addProcesses(processes: List[Process]) = processQueue = processQueue enqueue processes

  override def nextProcess: Run = processQueue dequeue match {
    case (process, queue) =>
      processQueue = queue
      val time = if (quantum <= process.remaining) quantum else process.remaining
      Run(process, time)
  }

  override def nonEmpty: Boolean = processQueue.nonEmpty

  override def processes: List[Process] = processQueue.toList
}