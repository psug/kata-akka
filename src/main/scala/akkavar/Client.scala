package akkavar

import akka.actor.Actor._
import java.util.Scanner
import java.net.InetAddress
import workers._

object Client {
  def main(args: Array[String]) {
    println("Starting client")

    
    val workFunc = ( in:DataWorkInput ) => DataWorkOutput( in.data )

    val serverHostName = InetAddress.getLocalHost.getHostName
    val worker = actorOf{ new Worker(  workFunc, serverHostName ) }.start()

    println("Any key stop ----------------------------------")
    new Scanner(System.in).nextLine()

    worker.stop()

  }
}
