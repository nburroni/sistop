import scala.concurrent.Future
import scala.util.Random

/**
  * Created by tombatto on 13/06/16.
  */
object App extends App {
  val size = 8
  val probability = 0.1
  val list = 0 to 300 toList
  val listP = 0 to 20 toList
  val pages : List[Page] = list.map(i=>Page(i,size))
  val processes : List[Process] = listP.map(i => Process(i,list,(Math.random()*100).toInt,probability))
  val memory = Memory(512,200,List())
  val virtualMemory = VirtualMemory(512,size,10000,pages)
  val algorithm = NRU(virtualMemory,memory,size)
  val memoryManager = MemoryManager(processes,algorithm)

  processes foreach { process =>
    println("Virtual A S "+process.id+":"+process.virtualAddressSpace)
    0 to process.quantityOfCalls foreach { i =>
      memoryManager.getPage(process.id, if(Math.random() > 0.85) Write(process.nextPage) else Read(process.nextPage))
    }
  }
  println(memoryManager.totalAccessTime)
  memoryManager.processPageFaults.foreach {
    case (id, i) => println(i)
  }
}
