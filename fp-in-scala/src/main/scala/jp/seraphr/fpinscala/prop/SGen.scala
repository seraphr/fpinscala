package jp.seraphr.fpinscala.prop

/**
  */
case class SGen[A](forSize: Int => Gen[A])
