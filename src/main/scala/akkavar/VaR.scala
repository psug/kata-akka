package akkavar

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

  def computeVaR(threshold: Double, contract: Option, mean: MarketData, variance: MarketData) : Double = {
    val data = generateMarketData(1000)(mean, variance)
    val prices = data.map { 
      mkt => { 
        val p = price(contract, mkt.spot, mkt.r, mkt.vol)
        println("%.4f;%.4f;%.4f;%.4f".format(mkt.spot,mkt.r,mkt.vol, p.premium))
        p.premium 
      }
    }
    val percent = new Percentile().evaluate(prices,threshold)
    println(threshold + "% = " + percent)
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
    val volRandom = gaussian.nextNormalizedDouble
    MarketData(spotRandom * variance.spot + mean.spot,
               rRandom * variance.r + mean.r,
               volRandom * variance.vol + mean.vol)
  }

}
