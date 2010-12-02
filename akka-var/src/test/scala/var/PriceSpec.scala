/**
 * Copyright 2010 Murex, S.A. All Rights Reserved.
 *
 * This software is the proprietary information of Murex, S.A.
 * Use is subject to license terms.
 */
package akkavar

import org.specs.runner.JUnit4
import org.specs._
import BlackScholes._

object PriceSpec extends Specification  {

  "A black-scholes pricer" should  {
    
    "Price a put and call option" in {
      val call = Call(360,100.0)
      val put = Put(360,100.0)
      price(call,100.0,0.05,0.01).premium must beCloseTo(0.3799,0.0001)
      price(put,100.0,0.05,0.01).premium must beCloseTo(price(call,100.0,0.05,0.01).premium,0.0001)
    }

  }
  
}

class PriceTest extends JUnit4(PriceSpec)
