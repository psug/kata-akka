package akkavar.work

import BlackScholes._
import akka.actor.Actor._
import akka.actor.ActorRef

import org.apache.commons.math.stat.descriptive.rank._
import akka.dispatch.Future
import akkavar.workers.{Worker, CentralDispatcher}




object parallelVaR extends VaR { 

  override def computeVaR(samples : Int, portfolio : Array[Options.Option], mean: MarketData, variance: MarketData) : Double = {
    remote.start()
    val centralDispatcher = actorOf[CentralDispatcher]
    remote.register("CentralDispatcher", centralDispatcher )

    val workFunc = this.computeVaRRaw _

    val workers : Array[ActorRef] = new Array(10)
    for(i <- 0 to 9)
      workers(i) = actorOf{ new Worker( workFunc, "localhost" ) }.start()

    var collectedFuture : Array[Future[Any]] = new Array(samples / 1000)

    for(i <- 0 to (samples / 1000)-1 )
      collectedFuture(i) = centralDispatcher !!! VaRInput(1000,1,portfolio,mean,variance)

    val collectedVaRResult = collectedFuture.map{ future => future.await; future.result.get.asInstanceOf[VaRResult] }

    remote.unregister("CentralDispatcher")
    remote.shutdown()

    workers foreach ( _.stop )
    centralDispatcher.stop

    // compute 1% percentile from subjobs
    val premiums = collectedVaRResult.flatMap (_.percentile)
    val percent = new Percentile().evaluate(premiums,1)
    val actualprice = prices(portfolio, mean.spot,mean.r,mean.vol).map(_.premium).foldLeft(0.0)(_ + _)
    (percent - actualprice) / actualprice
  }

}


object parallelCollectionVaR extends VaR {

  override def computeVaR(samples : Int, portfolio : Array[Options.Option], mean: MarketData, variance: MarketData) : Double = {

    val collectedVaRResult = (0 to (samples / 1000)-1 ).par.map( _ => computeVaRRaw( VaRInput(1000,1,portfolio,mean,variance) ) )


    // compute 1% percentile from subjobs
    val premiums = collectedVaRResult.flatMap (_.percentile).toArray
    val percent = new Percentile().evaluate(premiums,1)
    val actualprice = prices(portfolio, mean.spot,mean.r,mean.vol).map(_.premium).foldLeft(0.0)(_ + _)
    (percent - actualprice) / actualprice
  }

}
