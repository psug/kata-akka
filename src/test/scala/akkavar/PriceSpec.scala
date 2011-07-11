package akkavar

import work._
import work.Options._
import work.VaR._

import math._
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PriceSpec extends Specification  {

  val call = Call(360,100.0)
  val put = Put(360,100.0)
  val mean = MarketData(100,0.05,0.20)
  val variance = MarketData(20,0.001,0)

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
      backwardStep(pup,pdown,r,t, K)(layer1,layer)(0).premium must beCloseTo((0.5 * 0.05 + 0.5 * 0.06) * exp(-1),0.000001)
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
      val bs = BlackScholes.price(Put(150,50.0),50.0,0.10,0.4).premium
      val bt = price(100)(Put(150,50.0),50.0,0.10,0.4).premium 

      /*
       * Binomial tree converges to black-scholes so both prices must be equal
       */
      bs must beCloseTo(bt,0.01)
    }
  }

  "a Value at Risk computation" should { 

    import VaR._

    "provide generation of market data " in { 
      val mktdata = generateMarketData(1000)(MarketData(100,0.05,0.1), MarketData(10,0.01,0.02))
      mktdata.length must be_==(1000)
    }

    "provide 1% VaR for a given option" in { 
      val var1 = computeVaR(1000, 1, call, mean, variance)
      /*
       * all random variables used for generating scenarios are independent so the 1% var must
       * be equal to 1% of the mean price !
       */
      var1 must beCloseTo(-0.99,0.01)
    }
  }


  "distributing VaR computation" should {

    val portfolio : Array[Option] = for(maturity <- Range(30 ,720 , 30).toArray; strike <- Range(10, 200 , 10).toArray) yield Call(maturity, strike)
    val samples = 1000

    "compute VaR sequentially" in { 

      val start = System.nanoTime
      val vaR = computeVaRSequentially(samples, portfolio, mean, variance)
      val elapsedMs : Double = (System.nanoTime - start) / 1000000.0

      println("sequential VaR computation (" + samples +" scenarios, "+ portfolio.length +" positions) = " + "%.3f".format(elapsedMs))

      vaR must beCloseTo(-0.75,0.05)
    }

    "compute VaR with akka workers" in { 
      val start = System.nanoTime
      val vaR = computeVaRInParallel(samples, portfolio, mean, variance)
      val elapsedMs : Double = (System.nanoTime - start) / 1000000.0

      println("parallel VaR computation (" + samples +" scenarios, "+ portfolio.length +" positions) = " + "%.3f".format(elapsedMs))

      vaR must beCloseTo(-0.75,0.05)

    }
  }
}

