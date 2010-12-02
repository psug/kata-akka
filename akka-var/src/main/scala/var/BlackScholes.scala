/**
 * Copyright 2010 Murex, S.A. All Rights Reserved.
 *
 * This software is the proprietary information of Murex, S.A.
 * Use is subject to license terms.
 */
package akkavar

/**
 * Implements Black-scholes evaluation for options.
 */
object BlackScholes {

  val normalDistribution = org.apache.commons.math.distribution.DistributionFactory.newInstance.createNormalDistribution

  /**
   * Pricing characteristics of the option.
   * @param premium the price of the option
   * @param delta first derivative of price wrt. spot
   * @param gamma second derivative of price wrt. spot
   * @param vega ??
   * @param theta ??
   */
  case class OptionPrice(premium : Double, delta : Double, gamma : Double, vega : Double, theta : Double)

  /**
   * Compute price of an option.
   *
   * @param option the Option to price
   * @param spot   underlying spot value
   * @param r      risk-free annual interest rate
   * @param sigma  volatility
   */
  def price(option : Option, spot : Double, r : Double, sigma : Double) : OptionPrice = {
    val vst = sigma * Math.sqrt(option.maturityInYears)
    val d1  = option.sign * (Math.log(spot / option.strike) / vst + 0.5 * vst)
    val normd1 = normalDistribution.cumulativeProbability(d1)
    val d2  = d1 - option.sign * vst
    val delta = option.sign * normd1 / (1 + r) 
    val premium  = spot * delta - option.sign * option.strike * normalDistribution.cumulativeProbability(d2) / (1+r)
    val gamma = normd1 / ((1+r) * spot * vst)
    val vega = gamma * spot * spot * sigma * option.maturityInYears
    val theta = -0.5 * vega * sigma / option.maturityInYears
    OptionPrice(premium, delta, gamma, vega, theta)
  }
}

abstract class Option(val maturity : Long, val strike : Double) {
  val maturityInYears = maturity / 360
  val sign : Double
}

case class Call(override val maturity : Long, override val strike : Double) extends Option(maturity, strike) {
  override val sign = 1.0
}

case class Put(override val maturity : Long, override val strike : Double) extends Option(maturity, strike){
  override val sign = -1.0
}
