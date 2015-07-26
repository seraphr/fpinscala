package jp.seraphr.fpinscala.prop

/**
  */
case class SGen[A](forSize: Int => Gen[A]){
  def map[B](f: A => B): SGen[B] = SGen(n => (this forSize n).map(f))
  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n => (this forSize n).flatMap(f andThen (_ forSize n)))
}
