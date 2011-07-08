
package akkavar

import Options._
import math._

object BinomialTree { 

  def forwardStep(down : Double, up : Double)(layer : Array[Quote]) : Array[Quote] = { 
    val newLayer = new Array[Quote](layer.length + 1)
    for(i <- 0 to layer.length - 1) newLayer(i) = Quote(layer(i).spot * down,0.0)
    newLayer(layer.length) = Quote(layer(layer.length - 1).spot * up, 0.0)
    newLayer
  }

  def backwardStep(downProba : Double, upProba : Double, r : Double, t : Double, K : Double)(layer1 : Array[Quote], layer2 : Array[Quote]) : Array[Quote] = { 
    for(i <- 0 to layer1.length - 1) {  
      val premium = (downProba * layer2(i).premium + upProba *layer2(i + 1).premium) * exp(- r * t)
      layer1(i) = Quote(layer1(i).spot, max(K -layer1(i).spot, premium))
    }
    layer1
  }

  def evaluate (K : Double)(layer : Array[Quote]) : Array[Quote] = { 
    for(q <- layer) yield Quote(q.spot, max(K - q.spot, q.premium))
  }

  def price(layers: Int)(option : Option, spot : Double, r : Double, sigma : Double) : OptionPrice = {
    val time      = option.maturityInYears / layers
    val vst       = sigma * sqrt(time)
    val down      = exp(-vst)
    val up        = exp(vst)
    val growth    = exp(r * time)
    val upProba   = (growth - down) / (up - down)
    val downProba = 1 - upProba

    val delta	= 0.0
    val gamma	= 0.0
    val vega	= 0.0
    val theta	= 0.0

    var tree : List[Array[Quote]] = Nil
    val startLayer = Array(Quote(spot,0.0))
    var curLayer = startLayer
    val fwd = forwardStep(down,up)_
    val bkwd = backwardStep(downProba,upProba,r,time, option.strike)_
    for(i <- 1 to layers) { 
      val newLayer = fwd(curLayer)
      tree = curLayer :: tree
      curLayer = newLayer
    }

    curLayer = evaluate(option.strike)(curLayer)

    val updTree = for(layer <- tree) yield {
      val newLayer = bkwd(layer,curLayer)
      curLayer = layer
      newLayer
    }
    println(updTree.map(_.mkString(",")))
    OptionPrice(curLayer(0).premium, delta, gamma, vega, theta)
  }
}
