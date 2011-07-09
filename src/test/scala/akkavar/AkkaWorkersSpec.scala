package akkavar

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mutable.Specification
import akka.actor.Actor._
import akka.actor.Actor

/**
 * User: alag
 * Date: 7/9/11
 * Time: 7:33 PM
 */



@RunWith(classOf[JUnitRunner])
class AkkaWorkersSpec extends Specification {

  "central dispatcher" should  {
    "dispatch work to workers" in {
      remote.start()
      val centralDispatcher = actorOf[CentralDispatcher]
      remote.register("CentralDispatcher", centralDispatcher )



      val workFunc = ( in:WorkInput ) => WorkOutput( in.data )
      val worker = actorOf{ new Worker( workFunc, "localhost" ) }.start()

      val data = "Data"

      val actor = actorOf{ new Actor {
        def receive = {
          case wi:WorkInput =>
            val Some( workOuput ) = centralDispatcher !! wi
            println( "Test result got " + workOuput )
            self.reply( workOuput )
        }
      }}.start()

      val Some( workOuput ) = actor !!  WorkInput( data )

      actor.stop()

      remote.unregister("CentralDispatcher")
      remote.shutdown()

      worker.stop()
      centralDispatcher.stop()

      workOuput === WorkOutput( data )

    }
  }

}