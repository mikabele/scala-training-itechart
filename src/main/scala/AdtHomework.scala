import AdtHomework.BetComparator._
import AdtHomework.BetType._
import AdtHomework.Color._
import AdtHomework.GameResult._
import AdtHomework.Number.{NonZeroNumber, Zero}

import scala.util.Random

object AdtHomework {
  final case class Player(bet: BetType)

  sealed trait GameResult
  object GameResult {
    final case object Win extends GameResult
    final case object Lose extends GameResult
  }

  sealed trait Color
  object Color {
    final case object Black extends Color
    final case object Red extends Color
  }

  sealed trait Parity
  object Parity {
    final case object Even extends Parity
    final case object Odd extends Parity
  }

  sealed trait Number
  object Number {

    final case object Zero extends Number
    sealed abstract case class NonZeroNumber private (
      number: Int,
      color:  Color,
      parity: Parity
    ) extends Number

    object NonZeroNumber {

      private val reds = Set(1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36)

      private def determineColor(number: Int): Color = number match {
        case v if reds.contains(v) => Red
        case _                     => Black
      }

      private def determineParity(number: Int): Parity = number % 2 match {
        case 0 => Parity.Even
        case _ => Parity.Odd
      }

      def determineColumn(number: NonZeroNumber): Int = (number.number - 1) % 3 + 1

      def determineHalf(number: NonZeroNumber): Int = (number.number - 1) / 18 + 1

      def determineDozen(number: NonZeroNumber): Int = (number.number - 1) / 12 + 1

      def apply(number: Int): Either[String, NonZeroNumber] = if (number >= 1 && number <= 36)
        Right(
          new NonZeroNumber(
            number,
            determineColor(number),
            determineParity(number)
          ) {}
        )
      else
        Left("Invalid 'number' arg. It should be between 1 and 36")

    }

    def apply(number: Int): Either[String, Number] = if (number < 0 || number > 36)
      Left("Invalid 'number' arg. It should be between 0 and 36")
    else if (number == 0) Right(Zero)
    else
      NonZeroNumber(number)
  }

  sealed trait BetType
  object BetType {
    final case class Single(value: NonZeroNumber) extends BetType
    object Single {
      def apply(value: Int): Either[String, Single] = Number(value) match {
        case Right(nzn: NonZeroNumber) => Right(Single(nzn))
        case Right(Zero) => Left("You can't select Zero")
        case Left(e)     => Left(e)
      }
    }

    sealed abstract case class Split private (first: NonZeroNumber, second: NonZeroNumber) extends BetType
    object Split {
      def apply(first: NonZeroNumber, second: NonZeroNumber): Either[String, Split] = (first, second) match {
        case (f, s) if (f.number - 1) % 3 == 0 && s.number == f.number - 1 =>
          Left("You can't choose second number less than first one if first one takes place in the first column")
        case (f, s) if (f.number - 1) % 3 == 2 && s.number == f.number + 1 =>
          Left("You can't choose second number greater than first one if first one takes place in the third column")
        case (f, s) if Math.abs(f.number - s.number) != 1 && Math.abs(f.number - s.number) != 3 =>
          Left("First and second number don't touch")
        case _ => Right(new Split(first, second) {})
      }

      def apply(first: Int, second: Int): Either[String, Split] = (Number(first), Number(second)) match {
        case (Right(nzn1: NonZeroNumber), Right(nzn2: NonZeroNumber)) => Split(nzn1, nzn2)
        case (_, Right(Zero)) | (Right(Zero), _) => Left("You can't select Zero")
        case (Left(e1), _)                       => Left(e1)
        case (_, Left(e2))                       => Left(e2)
      }
    }

    sealed abstract case class Street private (begin: NonZeroNumber) extends BetType
    object Street {
      def apply(begin: NonZeroNumber): Either[String, Street] = if ((begin.number - 1) % 3 == 0)
        Right(new Street(begin) {})
      else
        Left("You should choose number from first column")

      def apply(begin: Int): Either[String, Street] = Number(begin) match {
        case Right(nzn: NonZeroNumber) => Street(nzn)
        case Right(Zero) => Left("You can't select Zero")
        case Left(e)     => Left(e)
      }
    }

    sealed abstract case class Corner private (begin: NonZeroNumber) extends BetType
    object Corner {
      def apply(begin: NonZeroNumber): Either[String, Corner] = if ((begin.number - 1) % 3 == 2)
        Left("You should choose number from the first or the second column")
      else
        Right(new Corner(begin) {})

      def apply(begin: Int): Either[String, Corner] = Number(begin) match {
        case Right(nzn: NonZeroNumber) => Corner(nzn)
        case Right(Zero) => Left("You can't select Zero")
        case Left(e)     => Left(e)
      }
    }

    sealed abstract case class DoubleStreet private (begin: NonZeroNumber) extends BetType
    object DoubleStreet {
      def apply(begin: NonZeroNumber): Either[String, DoubleStreet] = if ((begin.number - 1) % 3 == 0)
        Right(new DoubleStreet(begin) {})
      else
        Left("You should choose number from first column")

      def apply(begin: Int): Either[String, DoubleStreet] = Number(begin) match {
        case Right(nzn: NonZeroNumber) => DoubleStreet(nzn)
        case Right(Zero) => Left("You can't select Zero")
        case Left(e)     => Left(e)
      }
    }

    sealed abstract case class Basket(zeroCompanion: NonZeroNumber) extends BetType
    object Basket {
      def apply(zeroCompanion: NonZeroNumber): Either[String, Basket] =
        if (zeroCompanion.number != 1 && zeroCompanion.number != 3)
          Left("You should choose 1 or 3")
        else Right(new Basket(zeroCompanion) {})

      def apply(zeroCompanion: Int): Either[String, Basket] = Number(zeroCompanion) match {
        case Right(nzn: NonZeroNumber) => Basket(nzn)
        case Right(Zero) => Left("You can't select Zero")
        case Left(e)     => Left(e)
      }
    }
    final case class FirstFour() extends BetType

    sealed abstract case class Dozens private (dozen: Int) extends BetType
    object Dozens {
      private val validDozens = Set(1, 2, 3)
      def apply(dozen: Int): Either[String, Dozens] = if (validDozens.contains(dozen))
        Right(new Dozens(dozen) {})
      else Left("Invalid Dozen value")
    }

    sealed abstract case class Columns private (column: Int) extends BetType
    object Columns {
      private val validColumns = Set(1, 2, 3)
      def apply(column: Int): Either[String, Columns] = if (validColumns.contains(column))
        Right(new Columns(column) {})
      else
        Left("Invalid Column value")
    }

    final case class RedSnakeBet() extends BetType
    final case class ColorBet(color: Color) extends BetType
    final case class ParityBet(parity: Parity) extends BetType
    sealed abstract case class RangeBet private (half: Int) extends BetType
    object RangeBet {
      def apply(half: Int): Either[String, RangeBet] = if (half == 1 || half == 2)
        Right(new RangeBet(half) {})
      else
        Left("Invalid Range value")
    }
  }

  sealed trait BetComparator[T <: BetType] {
    protected def matchGeneratedNumber(
      ifZeroFunction: T => GameResult
    )(
      ifNonZeroFunction: (T, NonZeroNumber) => GameResult
    )(
      bet:          T,
      generatedNum: Number
    ): GameResult = generatedNum match {
      case Zero => ifZeroFunction(bet)
      case nonZero: NonZeroNumber => ifNonZeroFunction(bet, nonZero)
    }

    protected def loseIfZero: ((T, NonZeroNumber) => GameResult) => (T, Number) => GameResult =
      matchGeneratedNumber(_ => Lose)

    protected def winIfZero: ((T, NonZeroNumber) => GameResult) => (T, Number) => GameResult =
      matchGeneratedNumber(_ => Win)

    def compare(bet: T, generatedNum: Number): GameResult
  }
  object BetComparator {
    object SingleComparator extends BetComparator[Single] {
      override def compare(bet: Single, generatedNum: Number): GameResult =
        loseIfZero((b, n) => if (b.value.number == n.number) Win else Lose)(bet, generatedNum)
    }

    object SplitComparator extends BetComparator[Split] {
      override def compare(bet: Split, generatedNum: Number): GameResult =
        loseIfZero((b, n) => if (b.first.number == n.number || n.number == b.second.number) Win else Lose)(
          bet,
          generatedNum
        )
    }

    object StreetComparator extends BetComparator[Street] {
      override def compare(bet: Street, generatedNum: Number): GameResult =
        loseIfZero((b, n) => if (b.begin.number <= n.number && n.number < b.begin.number + 3) Win else Lose)(
          bet,
          generatedNum
        )
    }

    object CornerComparator extends BetComparator[Corner] {
      override def compare(bet: Corner, generatedNum: Number): GameResult = loseIfZero((b, n) =>
        if (
          b.begin.number == n.number || b.begin.number + 1 == n.number || b.begin.number + 3 == n.number || b.begin.number + 4 == n.number
        ) Win
        else Lose
      )(bet, generatedNum)
    }

    object DoubleStreetComparator extends BetComparator[DoubleStreet] {
      override def compare(bet: DoubleStreet, generatedNum: Number): GameResult =
        loseIfZero((b, n) => if (b.begin.number <= n.number && n.number < b.begin.number + 6) Win else Lose)(
          bet,
          generatedNum
        )
    }

    object BasketComparator extends BetComparator[Basket] {
      override def compare(bet: Basket, generatedNum: Number): GameResult =
        winIfZero((b, n) => if (n.number == 2 || b.zeroCompanion.number == n.number) Win else Lose)(bet, generatedNum)
    }

    object FirstFourComparator extends BetComparator[FirstFour] {
      override def compare(bet: FirstFour, generatedNum: Number): GameResult =
        winIfZero((_, n) => if (n.number == 1 || n.number == 2 || n.number == 3) Win else Lose)(bet, generatedNum)
    }

    object DozensComparator extends BetComparator[Dozens] {
      override def compare(bet: Dozens, generatedNum: Number): GameResult =
        loseIfZero((b, n) => if (b.dozen == NonZeroNumber.determineDozen(n)) Win else Lose)(bet, generatedNum)
    }

    object ColumnsComparator extends BetComparator[Columns] {

      override def compare(bet: Columns, generatedNum: Number): GameResult =
        loseIfZero((b, n) => if (b.column == NonZeroNumber.determineColumn(n)) Win else Lose)(bet, generatedNum)
    }

    object RedSnakeBetComparator extends BetComparator[RedSnakeBet] {

      private val snakeNumberList = List(1, 5, 9, 12, 14, 16, 19, 23, 27, 30, 32, 34)

      override def compare(bet: RedSnakeBet, generatedNum: Number): GameResult =
        loseIfZero((_, n) => if (n.color == Red && snakeNumberList.contains(n.number)) Win else Lose)(bet, generatedNum)
    }

    object ColorBetComparator extends BetComparator[ColorBet] {
      override def compare(bet: ColorBet, generatedNum: Number): GameResult =
        loseIfZero((b, n) => if (b.color == n.color) Win else Lose)(bet, generatedNum)
    }

    object ParityBetComparator extends BetComparator[ParityBet] {
      override def compare(bet: ParityBet, generatedNum: Number): GameResult =
        loseIfZero((b, n) => if (b.parity == n.parity) Win else Lose)(bet, generatedNum)
    }

    object RangeBetComparator extends BetComparator[RangeBet] {
      override def compare(bet: RangeBet, generatedNum: Number): GameResult =
        loseIfZero((b, n) => if (b.half == NonZeroNumber.determineHalf(n)) Win else Lose)(bet, generatedNum)
    }
  }

  def checkGameResult(
    generatedNum: Number
  )(
    player: Player
  ): GameResult = player.bet match {
    case bet: Single       => SingleComparator.compare(bet, generatedNum)
    case bet: Split        => SplitComparator.compare(bet, generatedNum)
    case bet: Street       => StreetComparator.compare(bet, generatedNum)
    case bet: Corner       => CornerComparator.compare(bet, generatedNum)
    case bet: DoubleStreet => DoubleStreetComparator.compare(bet, generatedNum)
    case bet: Basket       => BasketComparator.compare(bet, generatedNum)
    case bet: FirstFour    => FirstFourComparator.compare(bet, generatedNum)
    case bet: Dozens       => DozensComparator.compare(bet, generatedNum)
    case bet: Columns      => ColumnsComparator.compare(bet, generatedNum)
    case bet: RedSnakeBet  => RedSnakeBetComparator.compare(bet, generatedNum)
    case bet: ColorBet     => ColorBetComparator.compare(bet, generatedNum)
    case bet: ParityBet    => ParityBetComparator.compare(bet, generatedNum)
    case bet: RangeBet     => RangeBetComparator.compare(bet, generatedNum)
  }

  def generateNumber(): Either[String, Number] = {
    val randNum = Random.between(0, 37)
    Number(randNum)
  }

  def runGame(players: List[Player]): Either[String, List[GameResult]] = {
    generateNumber() match {
      case Right(num) =>
        Right(players.map(checkGameResult(num)))
      case Left(e) => Left(e)
    }
  }
}
