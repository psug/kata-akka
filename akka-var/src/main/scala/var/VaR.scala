package akkavar

import Options._
import BlackScholes._
import org.apache.commons.math.random._

/**
 * Computes Value-at-Risk for a given list of options.
 */
object VaR { 

  val rg = new MersenneTwister()
  rg.setSeed(17399225432l)
  val gaussian = new GaussianRandomGenerator(rg);

  case class MarketData(spot : Double, r : Double, vol : Double)

  def generateMarketData(count : Int)(mean : MarketData, sigma : MarketData): Array[MarketData] = { 
    for(i <- 0 to count) yield generateOnePoint(mean, sigma)
  }

  def generateOnePoint(mean : MarketData, sigma : MarketData) : MarketData = { 
    val spotRandom = gaussian.nextNormalizedDouble
    val rRandom = gaussian.nextNormalizedDouble
    val volRandom = gaussian.nextNormalizedDouble
    MarketData(spotRandom * sigma.spot + mean.spot,
               rRandom * sigma.r + mean.r,
               volRandom * sigma.vol + mean.vol)
  }
}
