package akkavar

import akkavar.workers.{Worker, WorkInput, WorkOutput}
import akka.actor.Actor._
import java.util.Scanner
import java.net.InetAddress


object Client {
  def main(args: Array[String]) {
    println("Starting client")

    
    val workFunc = ( in:WorkInput ) => WorkOutput( in.data )

    val serverHostName = InetAddress.getLocalHost.getHostName
    val worker = actorOf{ new Worker( workFunc, serverHostName ) }.start()

    println("Any key stop ----------------------------------")
    new Scanner(System.in).nextLine()

    worker.stop()

  }
}