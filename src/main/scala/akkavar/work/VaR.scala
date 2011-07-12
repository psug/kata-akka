package akkavar.work

import Options._
import BlackScholes._
import org.apache.commons.math._
import random._
import stat.descriptive.rank._
import akkavar.workers.{WorkOutput, WorkInput}

case class MarketData(spot: Double, r: Double, vol: Double)

case class VaRInput(samples : Int,             /* number of random MktData parameters to generate */
                    threshold: Double,         /* percentile to select */
                    contracts : Array[Option], /* portfolio to evaluate */
                    mean: MarketData,          /* mean distribution of market data parameters */
                    variance: MarketData       /* variance of marketdata parameters */
                  ) extends WorkInput

case class  VaRResult(percentile : Array[Double], /* low percentile prices */
                      threshold: Double           /* threshold value (ie. upper limit of percentile values) */
                    ) extends WorkOutput


trait VaR { 
  val rg = new MersenneTwister()
  rg.setSeed(17399225432l)
  val gaussian = new GaussianRandomGenerator(rg);

  type ComputeVaR = VaRInput => VaRResult

  def computeVaR(samples : Int, portfolio : Array[Option], mean: MarketData, variance: MarketData) : Double

  def computeVaRRaw(in : VaRInput) : VaRResult = {
    val data = generateMarketData(in.samples)(in.mean, in.variance)
    val simul = data.map { 
      mkt => { 
        val p = prices(in.contracts, mkt.spot, mkt.r, mkt.vol)
        val price = p.map( _.premium ).foldLeft(0.0)(_ + _)
// output computed price for graphing purpose 
//        println("%.4f\t%.4f\t%.4f\t%.4f".formatLocal(java.util.Locale.ENGLISH, mkt.spot,mkt.r,mkt.vol, price))
        price
        }
    }
    val percent = new Percentile().evaluate(simul,in.threshold)
    VaRResult(simul.filter(_ <= percent), percent)
  }

  /**
   * Generates a bunch of random market data parameters according to given mean and variance.
   *
   * @param count number of values to generate
   * @param mean mean values for spot, r and vol
   * @param variance variance for spot, r and vol
   * @return an array containing count randomly generated market data from normal distribution
   */
  def generateMarketData(count: Int)(mean: MarketData, variance: MarketData): Array[MarketData] =
    (1 to count).map { _ => generateOnePoint(mean, variance) }.toArray

  def generateOnePoint(mean: MarketData, variance: MarketData): MarketData = {
    val spotRandom = gaussian.nextNormalizedDouble
    val rRandom = gaussian.nextNormalizedDouble
    MarketData(spotRandom * variance.spot + mean.spot,
               rRandom * variance.r + mean.r,
               mean.vol)
  }
}

object sequentialVaR extends VaR {

  override def computeVaR(samples : Int, portfolio : Array[Option], mean: MarketData, variance: MarketData) : Double = {
    val VaRResult(simul, percent) = computeVaRRaw(VaRInput(samples, 1, portfolio, mean, variance))
    val actualprice = prices(portfolio, mean.spot,mean.r,mean.vol).map(_.premium).foldLeft(0.0)(_ + _)
    (percent - actualprice) / actualprice
  }

}
