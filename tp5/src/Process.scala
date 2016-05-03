/**
  * Created by nico on 02/05/16.
  */
case class Process(id: Int, priority: Int, arrivalTime: Int, resources: List[Resource]) {

  def remaining = resources.head.time

  def run(time: Int) =
    if (time >= remaining) copy(resources = resources.tail)
    else copy(resources = resources.head.decrease(time) :: resources.tail)

}
