package akkavar.work


import Options._
import math._
/**
 * Implements Black Scholes evaluation method for Options.
 */
object BlackScholes {

  val normalDistribution = new org.apache.commons.math.distribution.NormalDistributionImpl

  /**
   * Compute price of an option.
   *
   * @param option the Option to price
   * @param spot   underlying spot value
   * @param r      risk-free annual interest rate
   * @param sigma  volatility
   */
  def price_old(option : Option, spot : Double, r : Double, sigma : Double) : OptionPrice = {
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

  def prices(options : Array[Option],  spot : Double, r : Double, sigma : Double) : Array[OptionPrice] = options.map { o => price(o, spot, r, sigma) }

  def computeD1(option : Option, spot : Double, r : Double, sigma : Double) : Double =  {
    val denom = sigma * Math.sqrt(option.maturityInYears)
    val num = log(spot / option.strike) + (r + 0.5 * sigma * sigma) * option.maturityInYears
    num / denom
  }
  
  /**
   * d_2 = d_1 - \sigma \sqrt{t}
   * @param d1
   * @return
   */
  def computeD2(option : Option, spot : Double, r : Double, sigma : Double ) : Double = {
    val denom = sigma * Math.sqrt(option.maturityInYears)
    val num = log(spot / option.strike) + (r - 0.5 * sigma * sigma) * option.maturityInYears
    num / denom
  }
  
  /**
   * Compute the price of the call
   * C(S_0,K,r,t,\sigma) = S_0 \mathcal{N}(d_1) - K e^{-rt}\mathcal{N}(d_2)
   * \mathcal{N}(x) = \int_{-\infty}^{x} \frac{1}{\sqrt{2\pi}}e^{-\frac{1}{2}u^2} du
   *
   * @return
   * @throws MathException
   *
   * @param spot yield Allows to build a actual yield for each simulation
   */
  def price(option : Option, spot : Double, r : Double, sigma : Double) : OptionPrice = {
    val d1 = computeD1(option, spot, r, sigma)
    val d2 = computeD2(option, spot, r, sigma)
    val X = option.strike * exp(-1 * r * option.maturityInYears)
    if(option.sign > 0)
      OptionPrice(spot * normalDistribution.cumulativeProbability(d1) - X * normalDistribution.cumulativeProbability(d2), 0, 0,0,0)
    else 
      OptionPrice(- spot * normalDistribution.cumulativeProbability(- d1) + X * normalDistribution.cumulativeProbability(- d2), 0, 0,0,0)
  }
}

