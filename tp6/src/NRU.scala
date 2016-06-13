/**
  * Created by tombatto on 11/06/16.
  */
abstract class PageReplacementAlgorithm(virtualMemory: VirtualMemory, memory: Memory, pageSize: Int) {
  def getPage(access: Access): (Int,Boolean) = {
    val virtualPages = virtualMemory.pageMap
    val memoryPages = memory.pageMap
    val id = access.pageId

    memoryPages.get(id) match {
      case None =>
        virtualPages.get(id) match {
          case Some(page) =>
            onPageFault(access)
            (memory.accessTime + virtualMemory.accessTime, true)
        }
      case Some(page) =>
        onSuccess(access)
        (memory.accessTime, false)
    }
  }

  def onSuccess(access: Access)
  def onPageFault(access: Access)
}

case class NRU(virtualMemory: VirtualMemory, memory: Memory, pageSize: Int) extends PageReplacementAlgorithm(virtualMemory, memory, pageSize){
  var accesses = 0

  case class Category(r: Boolean, m: Boolean) extends Ordered[Category] {
    import scala.math.Ordered.orderingToOrdered
    override def compare(that: Category): Int = (this.r, this.m) compare (that.r, that.m)
  }

  var pageTable = memory.pageMap map {
    case (id, page) => id -> Category(r = false, m = false)
  }


  override def onSuccess(access: Access) = {
    access match {
      case Read(page) => pageTable += (access.pageId -> Category(r = true, m = false))
      case Write(page) => pageTable += (access.pageId -> Category(r = true,m = true))
    }
    increaseAccesses
  }

  def increaseAccesses = {
    accesses += 1
    if(accesses >= 10){
      pageTable = pageTable.map {
        case (id,category) => id -> Category(r = false, m = category.m)
      }
      accesses = 0
    }
  }

  override def onPageFault(access: Access) = {
    val pageId = access.pageId
    if((memory.size/pageSize).toInt <= memory.pages.size) {
      val toRemoveId = pageTable.groupBy(_._2).toList.sortBy(_._1).head._2.head._1
      removePage(toRemoveId)
    }
    addPage(pageId)
    increaseAccesses
  }

  def addPage(pageId: Int): Unit = {
    pageTable += pageId -> Category(r = true, m = false)
    memory.addPage(Page(pageId,pageSize))
  }

  def removePage(toRemoveId: Int): Unit = {
    pageTable -= toRemoveId
    memory.removePage(toRemoveId)
  }
}


case class FIFO(virtualMemory: VirtualMemory, memory: Memory, pageSize: Int) extends PageReplacementAlgorithm(virtualMemory, memory, pageSize){

  override def onSuccess(access: Access) = {

  }

  override def onPageFault(access: Access) = {
    val page = Page(access.pageId, pageSize)
    val toRemoveId = memory.pages.head.id
    memory.removePage(toRemoveId)
    memory.addPage(page)
  }
}

case class SecondChance(virtualMemory: VirtualMemory, memory: Memory, pageSize: Int) extends PageReplacementAlgorithm(virtualMemory, memory, pageSize){

  case class Category(r: Boolean) extends Ordered[Category] {
    import scala.math.Ordered.orderingToOrdered
    override def compare(that: Category): Int = (this.r) compare (that.r)
  }

  var pageTable = memory.pageMap map {
    case (id, page) => id -> Category(r = false)
  }


  override def onSuccess(access: Access) = {
    pageTable += (access.pageId -> Category(r = true))
  }

  override def onPageFault(access: Access) = {
    val page = Page(access.pageId, pageSize)
    val toRemoveId = findToRemove
    removePage(toRemoveId)
    addPage(page)
  }

  def findToRemove: Int = {
    val toRemove = pageTable.head
    if (toRemove._2.r) {
      pageTable -= toRemove._1
      pageTable += toRemove._1 -> Category(false)
      findToRemove
    } else {
      toRemove._1
    }
  }

  def addPage(page: Page): Unit = {
    pageTable += page.id -> Category(r = true)
    memory.addPage(page)
  }

  def removePage(toRemoveId: Int): Unit = {
    pageTable -= toRemoveId
    memory.removePage(toRemoveId)
  }
}

case class Clock(virtualMemory: VirtualMemory, memory: Memory, pageSize: Int) extends PageReplacementAlgorithm(virtualMemory, memory, pageSize){
  var handId = 0

  case class Category(r: Boolean) extends Ordered[Category] {
    import scala.math.Ordered.orderingToOrdered
    override def compare(that: Category): Int = (this.r) compare (that.r)
  }

  var pageTable = memory.pageMap map {
    case (id, page) => id -> Category(r = false)
  }


  override def onSuccess(access: Access) = {
    pageTable += (access.pageId -> Category(r = true))
  }

  override def onPageFault(access: Access) = {
    val page = Page(access.pageId, pageSize)
    val toRemoveId = findToRemove
    removePage(toRemoveId)
    addPage(page)
  }

  def findToRemove: Int = {
    val toRemove = pageTable.toList(handId)
    if (toRemove._2.r) {
      pageTable -= toRemove._1
      pageTable += toRemove._1 -> Category(false)
      handId += 1
      if(handId >= pageTable.size) handId = 0
      findToRemove
    } else {
      toRemove._1
    }
  }

  def addPage(page: Page): Unit = {
    pageTable += page.id -> Category(r = true)
    memory.addPage(page)
  }

  def removePage(toRemoveId: Int): Unit = {
    pageTable -= toRemoveId
    memory.removePage(toRemoveId)
  }
}
