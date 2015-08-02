package jp.seraphr.fpinscala.prop

/**
 */
object PropsMain extends App {
  Props.listMax()
  Props.listSort()
}

object Props {
  import Gen._
  import Prop._

  def listMax() = {
    val tSmallInt = choose(-10, 10)
    val tMaxProp = forAll(listOf1(tSmallInt)) { tList =>
      val tMax = tList.max
      !tList.exists(_ > tMax)
    }

    run(tMaxProp)
  }

  def listSort() = {
    val tSmallInt = choose(-10, 10)
    val tSortProp = forAll(listOf1(tSmallInt)) {
      case xs @ List() =>
        xs.sorted.isEmpty
      case xs @ List(_) =>
        xs.sorted == xs
      case tList =>
        val tSorted = tList.sorted

        tSorted.sliding(2).forall(xs => xs(0) <= xs(1))
    }

    run(tSortProp)
  }
}