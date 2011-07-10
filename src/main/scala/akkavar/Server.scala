package akkavar

import akka.actor.Actor._
import workers.{WorkInput, CentralDispatcher}
import java.util.Scanner
import java.net.InetAddress


object Server {
  def main(args: Array[String]) {
    println("Starting server")

    remote.start( InetAddress.getLocalHost.getHostName, 2552 )
    
    val centralDispatcher = actorOf[CentralDispatcher]
    remote.addListener(centralDispatcher)
    remote.register("CentralDispatcher", centralDispatcher )

    println("Waiting clients....----------------------------------")
    new Scanner(System.in).nextLine()
    println("Start computing....----------------------------------")

    val Some( workOuput ) = centralDispatcher !!  WorkInput( "Data" )
    println("Result " + workOuput )

    println("Any key stop ----------------------------------")
    new Scanner(System.in).nextLine()

    remote.unregister("CentralDispatcher")
    remote.shutdown()

    centralDispatcher.stop()

  }
}