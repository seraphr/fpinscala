package jp.seraphr.fpinscala.monoid

/**
 */
object WordCount {
  trait WC
  sealed case class Stub(chars: String) extends WC
  sealed case class Part(lStub: String, words: Int, rStub: String) extends WC

  def stub(chars: String): WC = Stub(chars)
  def part(lStub: String, words: Int, rStub: String): WC = Part(lStub, words, rStub)

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(l: WC, r: WC): WC = (l, r) match {
      case (Stub(l), Stub(r))                   => Stub(l + r)
      case (Stub(l), Part(rl, wc, rr))          => Part(l + rl, wc, rr)
      case (Part(ll, wc, lr), Stub(r))          => Part(ll, wc, lr)
      case (Part(ll, c1, ""), Part("", c2, rr)) => Part(ll, c1 + c2, rr)
      case (Part(ll, c1, _), Part(_, c2, rr)) => Part(ll, c1 + c2 + 1, rr)
    }
    override def zero: WC = stub("")
  }
}
