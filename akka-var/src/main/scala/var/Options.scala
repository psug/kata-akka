/**
 * Copyright 2010 Murex, S.A. All Rights Reserved.
 *
 * This software is the proprietary information of Murex, S.A.
 * Use is subject to license terms.
 */
package akkavar


object Options { 

  /**
   * Pricing characteristics of the option.
   * @param premium the price of the option
   * @param delta first derivative of price wrt. spot
   * @param gamma second derivative of price wrt. spot
   * @param vega first derivative of price wrt. volatility
   * @param theta first derivative of price wrt. time
   */
  case class OptionPrice(premium : Double, delta : Double, gamma : Double, vega : Double, theta : Double)

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

}

