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


class WorkInput
class WorkOutput
case object RegisterWorker

case class DataWorkInput( data:String ) extends WorkInput
case class DataWorkOutput( data:String ) extends WorkOutput


class Worker[IN<:WorkInput, OUT<:WorkOutput]( f : IN => OUT, centralDispatcherHost:String ) extends Actor {

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
    case wi:IN => println( "Got Work to do: " + wi ); self.reply( f( wi ) )
  }
}


class CentralDispatcher extends Actor {

  val workers = new ArrayBuffer[ActorRef]

  def receive = {
    case input:WorkInput =>
      val worker = workers( Random.nextInt( workers.size ) )
      println( "Dispatch to " + worker )
      val future = worker !!! input

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
