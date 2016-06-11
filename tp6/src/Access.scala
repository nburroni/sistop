/**
  * Created by tombatto on 11/06/16.
  */
trait Access {
  def page: Page
}

case class Read(page: Page) extends Access

case class Write(page: Page) extends Access
