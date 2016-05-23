/**
  * Created by nico on 02/05/16.
  */
case class Scheduler(processes: List[Process], algorithm: SchedulingAlgorithm, outputName: String) {

  var clock = Clock()

  var pending = processes.sortBy(_.arrivalTime)
  var blocked = List[Process]()
  var finished = List[Process]()

  val drawing = Drawing(outputName)

  processes.sortBy(_.id).foreach( p => drawing.addProcess(p.id) )

  def start = {
    clockTicks(pending.head.arrivalTime)
    println(s"Starting scheduler with clock on ${clock.time}ms")
    println("Pending: ")
    printList(pending)

    while (notDone) {
      println()

      if (algorithm.nonEmpty) {
        val Run(process, time) = algorithm.nextProcess
        drawing.processRun(id = process.id, time = time)
        println(s"Run: $process ${time}ms")
        clockTicks(time)
        val newProcess = process.run(time)
        newProcess.resources.headOption match {
          case Some(CPU(remaining)) => algorithm.addProcess(newProcess)
          case Some(IO(remaining)) => blocked = newProcess :: blocked
          case _ => finished = newProcess :: finished
        }

        println("Ready: ")
        printList(algorithm.processes)
        println("Finished: ")
        printList(finished)
      } else {
        var blockedTime = 99999999
        var pendingTime = 99999999
        if (blocked.nonEmpty) blockedTime = blocked.minBy(_.remaining).remaining
        if (pending.nonEmpty) pendingTime = pending.minBy(_.arrivalTime).arrivalTime
        clockTicks(Math.min(blockedTime, pendingTime))
      }
      println(s"Clock: ${clock.time}")
      println("Pending: ")
      printList(pending)
    }
    println("\nFinished scheduling all the processed processes processed by processing the non-processed processes turning them into processed processes... Bitch.\n")
    drawing.write
  }

  def clockTicks(ticks: Int) = {
    val (ready, stillPending) = pending map {
      case p: Process => p.copy(arrivalTime = if (p.arrivalTime - ticks <= 0) 0 else p.arrivalTime - ticks)
    } partition(_.arrivalTime <= 0)
    pending = stillPending

    val (unblocked, stillBlocked) = blocked.map { p =>
//      drawing.processIO(p.id, ticks, clock.time)
      p.run(ticks)
    }.takeWhile(!_.done).partition(_.nextResource.isInstanceOf[CPU])
    blocked = stillBlocked

    algorithm addProcesses (ready ++ unblocked)
    println("Blocked: ")
    printList(blocked)

    clock = clock.increase(ticks)
  }

  def notDone: Boolean = pending.nonEmpty || blocked.nonEmpty || algorithm.nonEmpty

  def printList[A](l: List[A]) = l.foreach(x => println(s"\t$x"))
}

case class Clock(time: Int = 0) {
  def increase(amount: Int) = copy(time = time + amount)
}