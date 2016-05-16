/**
  * Created by nico on 02/05/16.
  */
case class Scheduler(processes: List[Process], algorithm: SchedulingAlgorithm) {

  var clock = Clock()

  var pending = processes.sortBy(_.arrivalTime)
  var blocked = List[Process]()
  var finished = List[Process]()

  def start = {
    clockTicks(pending.head.arrivalTime)

  }

  def clockTicks(ticks: Int) = {
    clock = clock.increase(ticks)
    pending = pending map {
      case p: Process => p.copy(arrivalTime = p.arrivalTime - ticks)
    }
  }

}

case class Clock(time: Int = 0) {
  def increase(amount: Int) = copy(time = time + amount)
}