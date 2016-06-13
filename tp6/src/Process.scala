/**
  * Created by tombatto on 11/06/16.
  */
case class Process(id: Int, virtualAddressSpace: List[Int], quantityOfCalls: Int, probability: Double) {
  var currentPage = 0

  def nextPage() : Int = {
    if(Math.random() > probability) currentPage = virtualAddressSpace((Math.random()*virtualAddressSpace.size).toInt)
    currentPage
  }
}
