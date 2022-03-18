import Exercise1.Workspace.isCe
import MoreImplicitParameters.Workspace.MyLuckyNumber

import scala.language.implicitConversions
import scala.util.Try

object Exercise1 {
  import java.time.Instant

  val CommonEraStart: Instant = Instant.parse("0000-01-01T00:00:00.000Z")

  object Implicits {
    implicit class RichInstant(instant: Instant) {
      def isBce: Boolean = instant.isBefore(CommonEraStart)
    }
//      implicit def isBce(instant: Instant): Boolean = instant.isAfter(CommonEraStart)
  }

  object Workspace {
    import Implicits._
    //use isBce extension method to implement this one
    def isCe(instant: Instant): Boolean = !instant.isBce
  }

  def main(args: Array[String]): Unit = {
    val test = Instant.parse("2002-01-01T00:00:00.000Z")
    println(isCe(test))
  }
}

object MoreImplicitParameters {
  //Let's call this thing a type-class!
  trait Show[-T] {
    def apply(value: T): String
  }

  /*
  This is equivalent to def show[T](value: T)(implicit show: Show[T]): String = ...
  with the difference that the implicit argument is not named and can be obtained only using 'implicitly'.
  : Show syntax is called "a context bound"
   */
  def show[T: Show](value: T): String =
    implicitly[Show[T]].apply(value)

  object syntax {
    //our old friend implicit conversion but now with an implicit value requirement
    implicit class ShowOps[T: Show](inner: T) {
      def show: String = MoreImplicitParameters.show(inner)
    }
  }

  object instances {
    /*
    Type-classes provide a way to create generic logic which can be extended to work on any type.
    Here we extend all the possible logic working on Show, to work on some standard library types.
     */

    //for String's
    implicit val stringShow: Show[String] = (value: String) => value
    //for Int's
    implicit val intShow: Show[Int] = (value: Int) => value.toString
    //even for any Seq[T] where T itself has a Show instance
    implicit def seqShow[T: Show]: Show[Seq[T]] =
      (value: Seq[T]) => value.map(show(_)).mkString("(", ", ", ")")
  }

  object Workspace {
    import instances._
    import syntax._

    /*
    And here we extend all the possible logic working on Show, to work on our custom types!
     */
    case class MyLuckyNumber(value: Int)
    object MyLuckyNumber {
      implicit val myLuckyNumberShow: Show[MyLuckyNumber] =
        (luckyNumber: MyLuckyNumber) => s"lucky ${luckyNumber.value}"
    }

    def showEverything(): Unit = {
      println(42.show)
      println("hello!".show)
      println(Seq("I", "am", "a", "ghost").show)
      println(Seq(1, 2, 3, 4, 5).show)
      println(Seq(MyLuckyNumber(13), MyLuckyNumber(99)).show)
    }
  }
}

/*
Exercise 2.
Let us create a reverseShow method which should be defined for any T which has a Show type-class instance
 */
object Exercise2 {
  //change the method signature accordingly
  import MoreImplicitParameters._
  import MoreImplicitParameters.instances._
  import MoreImplicitParameters.syntax._

  def reverseShow[T: Show](value: T): String = value.show.reverse

  implicit class RichShowOps[T: Show](value: T) {
    def rShow: String = reverseShow(value)
  }

  def main(args: Array[String]): Unit = {
    println(42.rShow)
    println("hello!".rShow)
    println(Seq("I", "am", "a", "ghost").rShow)
    println(Seq(1, 2, 3, 4, 5).rShow)
    println(Seq(MyLuckyNumber(13), MyLuckyNumber(99)).rShow)
  }
}

object Exercise3 {

  /** Amount of years since the invention of the
    * hyper-drive technology (we are certainly in negative values at the moment).
    */
  case class HDEYears(value: Long)

  /*
  should be defined on any T which has Ordering[T] and return second biggest value from the sequence
  if it exists
  should work on our custom HDEYears
  change the signature accordingly, add implicit instances if needed
   */
  implicit val orderingHDEYears: Ordering[HDEYears] = new Ordering[HDEYears] {
    override def compare(x: HDEYears, y: HDEYears): Int = x.value.compare(y.value)
  }

  def secondBiggestValue[T](values: Seq[T])(implicit order: Ordering[T]): Option[T] = Try(
    values.sorted.takeRight(2).head
  ).toOption

  /** Custom number type!
    * For now it just wraps a Float but more interesting stuff could come in the future, who knows...
    */
  case class CustomNumber(value: Float)

  implicit val customNumberNumeric: Numeric[CustomNumber] = new Numeric[CustomNumber] {
    override def plus(x: CustomNumber, y: CustomNumber): CustomNumber = CustomNumber(x.value + y.value)

    override def minus(x: CustomNumber, y: CustomNumber): CustomNumber = ???

    override def times(x: CustomNumber, y: CustomNumber): CustomNumber = ???

    override def negate(x: CustomNumber): CustomNumber = ???

    override def fromInt(x: Int): CustomNumber = ???

    override def parseString(str: String): Option[CustomNumber] = ???

    override def toInt(x: CustomNumber): Int = ???

    override def toLong(x: CustomNumber): Long = ???

    override def toFloat(x: CustomNumber): Float = ???

    override def toDouble(x: CustomNumber): Double = ???

    override def compare(x: CustomNumber, y: CustomNumber): Int = ???
  }
  /*
  should be defined on any T which has Summable[T], should return sum value if it can be obtained
  should work on our custom CustomNumber
  change the signature accordingly, add implicit instances if needed
   */
  def sum[T](values: Seq[T])(implicit num: Numeric[T]): Option[T] = Try(values.sum).toOption

  def main(args: Array[String]): Unit = {
    val yearsSeq = Seq(HDEYears(10000L), HDEYears(5000L), HDEYears(20000L), HDEYears(40000L))
    println(secondBiggestValue(yearsSeq))
    val numSeq = Seq(CustomNumber(10000f), CustomNumber(5000f), CustomNumber(20000f), CustomNumber(40000f))
    println(sum(numSeq))
  }
}

object Exercise4 {
  /*
  Generic foldLeft!
  F[_] - type constructor with a single type argument, like List[T], Option[T], etc.
  Types which are parameterized using type constructors called higher-kinded types (HKT)
  Foldable here is a HKT
   */
  trait Foldable[F[_]] {
    def foldLeft[T, S](ft: F[T], s: S)(f: (S, T) => S): S
  }

  implicit val optionFoldable: Foldable[Option] = new Foldable[Option] {
    override def foldLeft[T, S](ft: Option[T], s: S)(f: (S, T) => S): S =
      ft match {
        case None    => s
        case Some(t) => f(s, t)
      }
  }
  implicit val listFoldable: Foldable[List] = new Foldable[List] {
    override def foldLeft[T, S](ft: List[T], s: S)(f: (S, T) => S): S =
      ft.foldLeft(s)(f)
  }

  case class Triple[T](
    v1: T,
    v2: T,
    v3: T,
  )

  /*
  Part 1.
  Define an Foldable instance for Triple (should behave like a collection of 3 elements)
   */

  implicit val tripleFoldable: Foldable[Triple] = new Foldable[Triple] {
    override def foldLeft[T, S](ft: Triple[T], s: S)(f: (S, T) => S): S = f(f(f(s, ft.v1), ft.v2), ft.v3)
  }
  /*
  Part 2.
  Define another type-class - Summable[T] which should give us methods:
  - def plus(left: T, right: T): T
  - def zero: T
  Define Summable[T] instances for:
  - any T which has the standard library Numeric[T] type-class provided
  - Set[S] - zero should be Set.empty and plus should merge sets with + operation
   */

  /*
  Part 3.
  And finally - define generic collection sum method which works on any F[T]
  where F is Foldable (F[_]: Foldable) and T is Summable (T: Summable)!
  def genericSum... - work out the right method signature, should take F[T] and return T
   */
}
