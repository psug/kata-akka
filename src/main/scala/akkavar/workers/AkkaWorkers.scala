package akkavar.workers

import akka.actor.Actor._
import akka.actor.{ActorRef, Actor}
import util.Random
import collection.mutable.ArrayBuffer
import java.net.InetAddress

/**
 * User: alag
 * Date: 7/9/11
 * Time: 7:17 PM
 */


case class WorkInput( data:String )
case class WorkOutput( data:String )
case object RegisterWorker


class Worker( f : WorkInput => WorkOutput, centralDispatcherHost:String ) extends Actor {

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
    case wi:WorkInput => println( "Got Work to do: " + wi ); self.reply( f( wi ) )
  }
}


class CentralDispatcher extends Actor {

  val workers = new ArrayBuffer[ActorRef]

  def receive = {
    case WorkInput( data ) =>
      val worker = workers( Random.nextInt( workers.size ) )
      println( "Dispatch to " + worker )
      val future = worker !!! WorkInput( data )

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
