package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] =
    Signal(math.pow(b(), 2) - 4 * a() * c())


  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
//    val sqrtDelta = Signal(math.sqrt(delta()))
//
//    Signal (
//      if (delta() < 0) Set()
//      else
//        Set(
//          (-b() + sqrtDelta()) / 2 * a(),
//          (-b() - sqrtDelta()) / 2 * a()
//      )
//    )
    Signal(
      Set ( ( -b() + math.sqrt(delta()) ) / 2 * a(),
            ( -b() - math.sqrt(delta()) ) / 2 * a() )
    )
  }
}
