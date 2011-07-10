package akkavar

import akka.actor.Actor._
import akka.actor.{ActorRef, Actor}
import util.Random
import collection.mutable.ArrayBuffer
import java.lang.annotation.Target


/**
 * User: alag
 * Date: 7/9/11
 * Time: 7:17 PM
 */


case class WorkInput( data:String )
case class WorkOutput( data:String )
case object RegisterWorker


class Worker( f : WorkInput => WorkOutput, centralDispatcherHost:String ) extends Actor {

  override def preStart(){
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
      val future = workers( Random.nextInt( workers.size ) ) !!! WorkInput( data )

      self.senderFuture.foreach{
        senderFutur =>
         senderFutur.completeWith( future )
      }



    case RegisterWorker =>
      println( "Register " + self.sender )
      workers += self.sender.get
  }

}
