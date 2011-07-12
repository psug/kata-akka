package akkavar.work

import BlackScholes._
import akka.actor.Actor._
import akka.actor.Actor
import akka.actor.{ActorRef, Actor}
import util.Random
import collection.mutable.ArrayBuffer

import java.net.InetAddress

import org.apache.commons.math.stat.descriptive.rank._
import akka.dispatch.Future

case object RegisterWorker

class Worker( f : VaRInput => VaRResult, centralDispatcherHost:String ) extends Actor {

  override def postStop(){
    println( "Stop ---------- " + self)
  }
  override def preStart(){
    // starting on another port than expecting server to avoid client/server collision
    remote.start( InetAddress.getLocalHost.getHostName,5225 )
    val centralDispatcher = remote.actorFor( "CentralDispatcher", centralDispatcherHost, 2552)
    centralDispatcher ! RegisterWorker
    println( "Register ---------- " + self)

  }

  def receive = {
    case wi:VaRInput => println( "Got Work to do: " + wi ); self.reply( f( wi ) )
  }
}


class CentralDispatcher extends Actor {

  val workers = new ArrayBuffer[ActorRef]

  def receive = {
    case in : VaRInput => 
      val worker = workers( Random.nextInt( workers.size ) )
      println( "Dispatch to " + worker )
      val future = worker !!! in

      self.senderFuture.foreach{
        senderFutur =>
          senderFutur.completeWith( future )
      }
    case RegisterWorker =>
      println( "Register " + self.sender +" - " + self.sender.get.homeAddress )
      self.sender.foreach( workers += _ )

    case m => println("UnManaged message: " + m )
  }

}

object parallelVaR extends VaR { 

  override def computeVaR(samples : Int, portfolio : Array[Options.Option], mean: MarketData, variance: MarketData) : Double = {
    remote.start()
    val centralDispatcher = actorOf[CentralDispatcher]
    remote.register("CentralDispatcher", centralDispatcher )

    val workFunc = this.computeVaRRaw _

    val workers : Array[ActorRef] = new Array(10)
    for(i <- 0 to 9)
      workers(i) = actorOf{ new Worker( workFunc, "localhost" ) }.start()

    var collectedOutput : Array[Future[Any]] = new Array(samples / 1000)



    for(i <- 0 to (samples / 1000)-1)
      collectedOutput(i) = centralDispatcher !!! VaRInput(1000,1,portfolio,mean,variance)



    remote.unregister("CentralDispatcher")
    remote.shutdown()

    workers foreach ( _.stop )
    centralDispatcher.stop

    // compute 1% percentile from subjobs
    val premiums = collectedOutput.map{ future => future.await; future.result.get.asInstanceOf[VaRResult] }.flatMap (_.percentile)
    val percent = new Percentile().evaluate(premiums,1)
    val actualprice = prices(portfolio, mean.spot,mean.r,mean.vol).map(_.premium).foldLeft(0.0)(_ + _)
    (percent - actualprice) / actualprice
  }

}
