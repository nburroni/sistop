/**
  * Created by nico on 16/05/16.
  */
object RunScheduler extends App {

  val processes = List(
    Process(id = 1, arrivalTime = 10, priority = 4, resources = List( CPU(5), IO(10), CPU(3) )),
    Process(id = 3, arrivalTime = 15, priority = 1, resources = List( CPU(2) )),
    Process(id = 2, arrivalTime = 10, priority = 2, resources = List( CPU(5), IO(10), CPU(6) ))
  )
//  Scheduler(processes = processes, algorithm = RoundRobinScheduling(quantum = 4)).start
//  Scheduler(processes = processes, algorithm = PriorityScheduling(quantum = 4)).start
  Scheduler(processes = processes, algorithm = ShortestJobFirstScheduling).start
//  Scheduler(processes = processes, algorithm = FirstComeFirstServeScheduling).start

}
