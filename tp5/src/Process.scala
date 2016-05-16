/**
  * Created by nico on 02/05/16.
  */
case class Process(id: Int, priority: Int, arrivalTime: Int, resources: List[Resource]) {

  def nextResource = resources.head
  def remaining = resources.head.time
  def done = resources == Nil
  def cpuTotal = resources.filter(_.isInstanceOf[CPU]).map(_.time).sum

  def run(time: Int) =
    if (time >= remaining) copy(resources = resources.tail)
    else copy(resources = resources.head.decrease(time) :: resources.tail)

  override def toString: String = s"Process { id: $id, priority: $priority, arrivalTime: $arrivalTime, resources: $resources } "
}
