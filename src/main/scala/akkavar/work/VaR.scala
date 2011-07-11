package akkavar.work

import Options._
import BlackScholes._
import org.apache.commons.math._
import random._
import stat.descriptive.rank._

/**
 * Computes Value-at-Risk for a given list of options.
 */
object VaR {

  val rg = new MersenneTwister()
  rg.setSeed(17399225432l)
  val gaussian = new GaussianRandomGenerator(rg);

  case class MarketData(spot: Double, r: Double, vol: Double)

  case class VaRInput(samples : Int,             /* number of random MktData parameters to generate */
                      threshold: Double,         /* percentile to select */
                      contracts : Array[Option], /* portfolio to evaluate */
                      mean: MarketData,          /* mean distribution of market data parameters */
                      variance: MarketData       /* variance of marketdata parameters */
                    )

  case class  VaRResult(percentile : Array[Double], /* low percentile prices */
                        threshold: Double           /* threshold value (ie. upper limit of percentile values) */
                      )

  type ComputeVaR = VaRInput => VaRResult

  /**
   * Same as above but VaR is computed in parallel.
   */
  def computeVaRInParallel(samples : Int, portfolio : Array[Option], mean: MarketData, variance: MarketData) : Double = {
    val VaRResult(simul, percent) = computeVaRRaw(VaRInput(samples, 1, portfolio, mean, variance))
    val actualprice = prices(portfolio, mean.spot,mean.r,mean.vol).map(_.premium).foldLeft(0.0)(_ + _)
    (percent - actualprice) / actualprice
  }

  /**
   * Compute 1% VaR of an array of options (eg. a Portolio) using samples size of marketdata and given mean and variance.
   * VaR is computed sequentially.
   */
  def computeVaRSequentially(samples : Int, portfolio : Array[Option], mean: MarketData, variance: MarketData) : Double = {
    val VaRResult(simul, percent) = computeVaRRaw(VaRInput(samples, 1, portfolio, mean, variance))
    val actualprice = prices(portfolio, mean.spot,mean.r,mean.vol).map(_.premium).foldLeft(0.0)(_ + _)
    (percent - actualprice) / actualprice
  }

  def computeVaRRaw(in : VaRInput) : VaRResult = {
    val data = generateMarketData(in.samples)(in.mean, in.variance)
    val simul = data.map { 
      mkt => { 
        val p = prices(in.contracts, mkt.spot, mkt.r, mkt.vol)
        val price = p.map( _.premium ).foldLeft(0.0)(_ + _)
        println("%.4f;%.4f;%.4f;%.4f".format(mkt.spot,mkt.r,mkt.vol, price))
        price
        }
    }
    val percent = new Percentile().evaluate(simul,in.threshold)
    VaRResult(simul.filter(_ <= percent), percent)
  }

  def computeVaR(samples : Int, threshold: Double, contract: Option, mean: MarketData, variance: MarketData) : Double = {
    val data = generateMarketData(samples)(mean, variance)
    val prices = data.map { 
      mkt => { 
        val p = price(contract, mkt.spot, mkt.r, mkt.vol)
        println("%.4f\t%.4f\t%.4f\t%.4f".format(mkt.spot,mkt.r,mkt.vol, p.premium))
        p.premium 
        }
    }
    val percent = new Percentile().evaluate(prices,threshold)
    val actualprice = price(contract, mean.spot,mean.r,mean.vol)
    (percent - actualprice.premium) / actualprice.premium
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
