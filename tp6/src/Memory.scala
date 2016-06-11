/**
  * Created by tombatto on 11/06/16.
  */
case class Memory(size: Int, accessTime: Int, pages: List[Page]) {

  var pageMap = pages.map(p => p.id -> p).toMap

  def addPage(page: Page) = {
    pageMap += (page.id -> page)
  }

  def removePage(id: Int) = {
    pageMap -= id
  }
}
