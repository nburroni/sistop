/**
  * Created by nico on 11/06/16.
  */
case class VirtualMemory(size: Int, pageSize: Int, accessTime: Int, pages: List[Page]) {

  var pageMap = pages.map(p => p.id -> p).toMap

}
