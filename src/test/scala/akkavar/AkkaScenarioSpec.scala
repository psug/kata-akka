package akkavar

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mutable.Specification
import akka.actor.Actor._
import akka.actor._
import akka.config.Supervision._

/**
 * User: alag
 * Date: 7/9/11
 * Time: 4:04 PM
 */


case class SimpleMessage( msg:String )
case object FailMessage


class SimpleWorker extends Actor {
  def receive = {
    case SimpleMessage( msg ) => processMessage( msg )
  }

  def processMessage( msg:String ){
    Thread.sleep(100)
    self.reply( SimpleMessage( msg ) )
  }
}

class FrostWorker extends SimpleWorker {
  override def processMessage( msg:String ){
    Thread.sleep(1000*1000)
  }
}
class FailWorker extends SimpleWorker {
  override def preRestart(reason: Throwable) = {
    println( "Prerestarting: " + reason  )
  }
  override def postRestart(reason: Throwable) = {
    println( "Postrestarting: " + reason  )
  }

  override def receive = {
    super.receive orElse { case FailMessage => throw new RuntimeException }
  }
}


@RunWith(classOf[JUnitRunner])
class AkkaScenarioSpec extends Specification {

  val simpleMessage = SimpleMessage( "Dude where is my car?" )

  "a simple actor" should  {
    "reply to a message sent out of an actor context" in {
      val actor = actorOf[SimpleWorker].start()

      val receive = actor !! simpleMessage
      // val receive = actor !! ( simpleMessage, 1000 )
      receive === Some( simpleMessage )
    }

    "reply to a message sent from spawn context" in {
      val actor = actorOf[SimpleWorker].start()
      val sync = new Object()

      var receive:Option[Any] = None
      spawn{
        receive = actor !! simpleMessage
        sync.synchronized{ sync.notify() }
      }
      sync.synchronized{ sync.wait() }

      receive === Some( simpleMessage )
    }


    "reply to a message sent from futur context" in {
      val actor = actorOf[SimpleWorker].start()

      val future = actor !!! simpleMessage
      future.value === None
      future.await
      future.value === Some( Right( simpleMessage ) )
    }



    "can be registered as remote actor" in {
      remote.start()
      remote.register("SimpleWorker", actorOf[SimpleWorker])


      val actor = remote.actorFor("SimpleWorker", "localhost", 2552)

      val receive = actor !! simpleMessage

      remote.unregister("SimpleWorker")
      remote.shutdown()

      receive === Some( simpleMessage )
    }
  }



  "a frost actor" should  {
    "timeout for a message sent out of an actor context" in {
      val actor = actorOf[FrostWorker].start()

      val receive = actor !! simpleMessage
      receive === None
    }
  }



  "a fail actor" should  {
    "fail for a fail message sent out of an actor context" in {
      val actor = actorOf[FailWorker].start()

      try {
        actor !! FailMessage
        failure
      } catch { case e => }
      val receive = actor !! simpleMessage
      receive === Some( simpleMessage )
//      receive === None

    }

    "fail for a fail message sent to remote" in {
      remote.start()
      remote.register("FailWorker", actorOf[FailWorker])


      val actor = remote.actorFor("FailWorker", "localhost", 2552)


      try {
        actor !! FailMessage
        failure
      } catch {
        case e =>
          println(e)
      }

      try {
        val receive = actor !! simpleMessage
        receive === Some( simpleMessage )
//        receive === None
      }
      finally {
        remote.unregister("FailWorker")
        remote.shutdown()

      }

    }


    "be supervised" in {
      val supervisor = Supervisor(
        SupervisorConfig(
          OneForOneStrategy( List(classOf[Exception]), 3, 10),
          Supervise( actorOf[FailWorker], Permanent ) :: Nil
          ) ).start



      val actor = actorOf[FailWorker].start()
      supervisor.link( actor )
      
      try {
        actor !! FailMessage
      } catch { case e => }


      val receive = actor !! simpleMessage

      supervisor.shutdown()

      receive === Some( simpleMessage )
    }
  }

}