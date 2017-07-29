package calculator

import math.sqrt

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]) =
    Signal(b()*b() - 4*a()*c())

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]) =
    Signal {
      if(delta() > 0)
        Set((-b() + sqrt(delta())) / 2,
            (-b() - sqrt(delta())) / 2)
      else if(delta() == 0)
        Set(-b() / 2)
      else
        Set()
    }
}
