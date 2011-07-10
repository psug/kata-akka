package akkavar

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mutable.Specification
import akka.actor.Actor._
import akkavar.workers.{Worker, WorkOutput, WorkInput, CentralDispatcher}

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
      remote.addListener(centralDispatcher)
      remote.register("CentralDispatcher", centralDispatcher )



      val workFunc = ( in:WorkInput ) => WorkOutput( in.data )
      val worker = actorOf{ new Worker( workFunc, "localhost" ) }.start()

      val data = "Data"

      val Some( workOuput ) = centralDispatcher !!  WorkInput( data )


      worker.stop()
      



      Thread.sleep(1000)
      remote.unregister("CentralDispatcher")
      remote.shutdown()

      centralDispatcher.stop()

      workOuput === WorkOutput( data )

    }
  }

}