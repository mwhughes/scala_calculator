package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal((b()*b()) - (4*a()*c()) )
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal( Set(  (computeDelta(a, b, c)()-b())/(2*a())
        , (-computeDelta(a, b, c)()-b())/(2*a()))
    )
  }
}
