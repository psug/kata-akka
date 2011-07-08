
package akkavar

import Options._
import math._
/**
 * Implements Black Scholes evaluation method for Options.
 */
object BlackScholes {

  val normalDistribution = org.apache.commons.math.distribution.DistributionFactory.newInstance.createNormalDistribution

  /**
   * Compute price of an option.
   *
   * @param option the Option to price
   * @param spot   underlying spot value
   * @param r      risk-free annual interest rate
   * @param sigma  volatility
   */
  def price(option : Option, spot : Double, r : Double, sigma : Double) : OptionPrice = {
    val vst	= sigma * sqrt(option.maturityInYears)
    val d1	= option.sign * (log(spot / option.strike) / vst + 0.5 * vst)
    val normd1	= normalDistribution.cumulativeProbability(d1)
    val d2	= d1 - option.sign * vst
    val delta	= option.sign * normd1 / (1 + r) 
    val premium = spot * delta - option.sign * option.strike * normalDistribution.cumulativeProbability(d2) / (1+r)
    val gamma	= normd1 / ((1+r) * spot * vst)
    val vega	= gamma * spot * spot * sigma * option.maturityInYears
    val theta	= -0.5 * vega * sigma / option.maturityInYears
    OptionPrice(premium, delta, gamma, vega, theta)
  }
}

