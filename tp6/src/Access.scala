/**
  * Created by tombatto on 11/06/16.
  */
trait Access {
  def pageId: Int
}

case class Read(pageId: Int) extends Access

case class Write(pageId: Int) extends Access
