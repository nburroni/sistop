/**
  * Created by tombatto on 11/06/16.
  */
case class MemoryManager(processes: List[Process], pageReplacementAlgorithm: PageReplacementAlgorithm){
  var totalAccessTime = 0
  var processPageFaults =  processes.map(p => p.id -> 0).toMap

  def getPage(processId: Int, access: Access) = {
    val (time, fault) = pageReplacementAlgorithm.getPage(access)
    totalAccessTime += time
    if (fault) {
      processPageFaults += (processId -> (processPageFaults.getOrElse(processId,0) + 1))
    }
  }
}
