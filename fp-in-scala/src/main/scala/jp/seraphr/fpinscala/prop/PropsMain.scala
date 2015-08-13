package jp.seraphr.fpinscala.prop

/**
 */
object PropsMain extends App {
  Props.listMax()
  Props.listSort()
  Props.booleanListSort()
  Props.constLengthListSort()
  Props.takeWhile()
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

  def constLengthListSort() = {
    val tSmallInt = choose(-2, 2)
    val tSortProp = forAll(listOfN(5, tSmallInt)) {
      case xs @ List() =>
        xs.sorted.isEmpty
      case xs @ List(_) =>
        xs.sorted == xs
      case tList =>
        val tSorted = tList.sorted

        tSorted.sliding(2).forall(xs => xs(0) <= xs(1))
    }

    run(tSortProp, testCases = 2048)
  }

  def booleanListSort() = {
    val tBoolean = boolean
    val tSortProp = forAll(listOfN(3, tBoolean)) { tList =>
      tList.sorted.sliding(2).forall(xs => xs(0) <= xs(1))
    }

    run(tSortProp)
  }

  def takeWhile() = {
    def isEven(a: Int) = a % 2 == 0
    val tSmallInt = choose(-10, 10)

    val tTakeWhileProp1 = forAll(listOf1(tSmallInt)) { tList =>
      tList.takeWhile(isEven).forall(isEven)
    }

    val tTakeWhileProp2 = forAll(listOf1(tSmallInt)) { tList =>
      (tList.takeWhile(isEven) ++ tList.dropWhile(isEven)) == tList
    }

    run(tTakeWhileProp1 && tTakeWhileProp2)
  }
}