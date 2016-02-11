package jp.seraphr.fpinscala.monoid

/**
 */
object WordCount {
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def stub(chars: String): WC = Stub(chars)
  def part(lStub: String, words: Int, rStub: String): WC = Part(lStub, words, rStub)

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(l: WC, r: WC): WC = (l, r) match {
      case (Stub(l), Stub(r))                   => Stub(l + r)
      case (Stub(l), Part(rl, wc, rr))          => Part(l + rl, wc, rr)
      case (Part(ll, wc, lr), Stub(r))          => Part(ll, wc, lr + r)
      case (Part(ll, c1, ""), Part("", c2, rr)) => Part(ll, c1 + c2, rr)
      case (Part(ll, c1, _), Part(_, c2, rr))   => Part(ll, c1 + c2 + 1, rr)
    }

    override def zero: WC = stub("")
  }

  def count(aString: String): Int = {
    def inner(aSubString: String): WC = aSubString match {
      case ""                 => wcMonoid.zero
      case " "                => part("", 0, "")
      case s if s.length == 1 => stub(s)
      case s =>
        val (l, r) = s.splitAt(s.length / 2)
        wcMonoid.op(inner(l), inner(r))
    }
    inner(aString) match {
      case Stub("")        => 0
      case Stub(_)         => 1
      case Part("", c, "") => c
      case Part("", c, _)  => c + 1
      case Part(_, c, "")  => c + 1
      case Part(_, c, _)   => c + 2
    }
  }
}
