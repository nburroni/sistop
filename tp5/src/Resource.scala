/**
  * Created by nico on 02/05/16.
  */
trait Resource {
  val time: Int

  def decrease(time: Int): Resource
}

case class CPU(time: Int) extends Resource {
   def decrease(amount: Int) = copy(time = time - amount)
}

case class IO(time: Int) extends Resource {
  def decrease(amount: Int) = copy(time = time - amount)
}