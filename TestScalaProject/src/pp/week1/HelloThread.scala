package pp.week1

class HelloThread extends Thread {
  override def run() {
    println("Hello ")
    println("world!")
  }
}

object HelloThread {

  private var uidCount = 0
  private val x = new AnyRef {}

  def getUniqueId(): Int = x.synchronized {
    uidCount = uidCount + 1
    uidCount
  }
  
  def startThread() = {
    val t = new Thread {
      override def run() {
        val uids = for (i <- 0 until 10) yield getUniqueId()
        println(uids)
      }
    }
    t.start()
    t
  }

  def main(args: Array[String]): Unit = {
/*    val t = new HelloThread()
    val s = new HelloThread()
    t.start()
    s.start()
    t.join()
    s.join()
*/
   startThread()
   startThread()
   startThread()
   startThread()
   startThread()
  }
}