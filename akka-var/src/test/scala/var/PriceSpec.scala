/**
 * Copyright 2010 Murex, S.A. All Rights Reserved.
 *
 * This software is the proprietary information of Murex, S.A.
 * Use is subject to license terms.
 */
package akkavar

import org.specs.runner.JUnit4
import org.specs._

import Options._

object PriceSpec extends Specification  {

  val call = Call(360,100.0)
  val put = Put(360,100.0)
  
  "A black-scholes pricer" should  {
    
    "Price a put and call option" in {
      import BlackScholes._

      price(call,100.0,0.05,0.01).premium must beCloseTo(0.3799,0.0001)
      price(put,100.0,0.05,0.01).premium must beCloseTo(price(call,100.0,0.05,0.01).premium,0.0001)
    }

  }

  "A binomial tree pricer" should {
    import BinomialTree._
    
    "a forward step creates a layer of size 2 from a layer of size 1" in { 
      val layer : Array[Double] = Array(1.0)
      val up = 1.1
      val down = 0.9
      forwardStep(down,up)(layer)(0) must beCloseTo(0.9,0.00001)
    }

    "a forward step creates a layer of size 3 from a layer of size 2" in { 
      val layer : Array[Double] = Array(0.9,1.1)
      val up = 1.1
      val down = 0.9
      forwardStep(down,up)(layer)(0) must beCloseTo(0.9 * 0.9,0.00001)
      forwardStep(down,up)(layer)(1) must beCloseTo(0.9 * 1.1,0.00001)
      forwardStep(down,up)(layer)(2) must beCloseTo(1.1 * 1.1,0.00001)
    }

    "Price a put option" in {
      price(5)(call,100.0,0.05,0.01).premium must beCloseTo(0.3799,0.0001)
    }
  }
  
}

class PriceTest extends JUnit4(PriceSpec)
