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
      val layer : Array[Quote] = Array(Quote(1.0,0.0))
      val up = 1.1
      val down = 0.9
      forwardStep(down,up)(layer)(0).spot must beCloseTo(0.9,0.00001)
    }

    "a forward step creates a layer of size 3 from a layer of size 2" in { 
      val layer : Array[Quote] = Array(Quote(0.9,0.0),Quote(1.1,0.0))
      val up = 1.1
      val down = 0.9
      forwardStep(down,up)(layer)(0).spot must beCloseTo(0.9 * 0.9,0.00001)
      forwardStep(down,up)(layer)(1).spot must beCloseTo(0.9 * 1.1,0.00001)
      forwardStep(down,up)(layer)(2).spot must beCloseTo(1.1 * 1.1,0.00001)
    }

    "a backward step creates a layer of size 1 from a layer on size 2" in {
      val layer : Array[Quote] = Array(Quote(0.9,0.05),Quote(1.1,0.06))
      val layer1 : Array[Quote] = Array(Quote(0.9,0.0))
      val pup = 0.5
      val pdown = 0.5
      val K = 0.1
      val r = 1
      val t = 1
      backwardStep(pup,pdown,r,t, K)(layer1,layer)(0).premium must beCloseTo((0.5 * 0.05 + 0.5 * 0.06) * Math.exp(-1),0.000001)
    }

    "evaluating a layer yields max(K - S, current premium)" in { 
      val layer : Array[Quote] = Array(Quote(0.9,0.2),Quote(0.91,0.05),Quote(1.1,0.0))
      val K = 1
      evaluate(K)(layer)(0).premium must beCloseTo(0.2,0.0000001)
      evaluate(K)(layer)(1).premium must beCloseTo(0.09,0.0000001)
      evaluate(K)(layer)(2).premium must beCloseTo(0.0,0.0000001)
    }

    "Price a put option" in {
      /*
       * Option characteristics comes from 'Options, Futures and Other derivatives', J.Hull, 7th Edition, p.411
       * tolerance in results is greater than usual because computations in book are truncated at 2 digits.
       */
      price(5)(Put(150,50.0),50.0,0.10,0.4).premium must beCloseTo(4.49,0.01)
    }
  }
  
}

class PriceTest extends JUnit4(PriceSpec)
