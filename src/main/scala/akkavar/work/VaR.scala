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
  
  /**
   * Compute 1% VaR of an array of options (eg. a Portolio) using samples size of marketdata and given mean and variance.
   * VaR is computed sequentially.
   */
  def computeVaRSequentially(samples : Int, portfolio : Array[Option], mean: MarketData, variance: MarketData) : Double = {
    val (simul, percent) = computeVaRRaw(samples, 1, portfolio, mean, variance)
    val actualprice = prices(portfolio, mean.spot,mean.r,mean.vol).map(_.premium).foldLeft(0.0)(_ + _)
    (percent - actualprice) / actualprice
  }

  
  def computeVaRRaw(samples : Int, threshold: Double, contracts : Array[Option], mean: MarketData, variance: MarketData) : (Array[Double], Double) = {
    val data = generateMarketData(samples)(mean, variance)
    val simul = data.map { 
      mkt => { 
        val p = prices(contracts, mkt.spot, mkt.r, mkt.vol)
        val price = p.map( _.premium ).foldLeft(0.0)(_ + _)
        println("%.4f;%.4f;%.4f;%.4f".format(mkt.spot,mkt.r,mkt.vol, price))
        price
        }
    }
    val percent = new Percentile().evaluate(simul,threshold)
    (simul.filter(_ <= percent), percent)
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
