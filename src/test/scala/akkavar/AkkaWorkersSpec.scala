package akkavar

import workers._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mutable.Specification
import akka.actor.Actor._

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

      val workFunc = ( in:DataWorkInput ) => DataWorkOutput( in.data )
      val worker = actorOf{ new Worker( workFunc, "localhost" ) }.start()

      val data = "Data"

      val Some( workOuput ) = centralDispatcher !!  DataWorkInput( data )


      remote.unregister("CentralDispatcher")
      remote.shutdown()

      worker.stop()
      centralDispatcher.stop()

      workOuput === DataWorkOutput( data )

    }
  }

}
