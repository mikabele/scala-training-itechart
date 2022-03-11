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

  sealed trait Dozen
  object Dozen {
    final case object FirstDozen extends Dozen
    final case object SecondDozen extends Dozen
    final case object ThirdDozen extends Dozen
  }

  sealed trait Column
  object Column {
    final case object FirstColumn extends Column
    final case object SecondColumn extends Column
    final case object ThirdColumn extends Column
  }

  sealed trait NumberRange
  object NumberRange {
    final case object Low extends NumberRange
    final case object High extends NumberRange
  }

  sealed trait Number
  object Number {

    final case object Zero extends Number
    sealed abstract case class NonZeroNumber private (
      number:      Int,
      color:       Color,
      parity:      Parity,
      column:      Column,
      numberRange: NumberRange,
      dozen:       Dozen
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

      private def determineColumn(number: Int): Column = (number - 1) % 3 match {
        case 0 => Column.FirstColumn
        case 1 => Column.SecondColumn
        case _ => Column.ThirdColumn
      }

      private def determineNumberRange(number: Int): NumberRange =
        if (number <= 18) NumberRange.Low else NumberRange.High

      private def determineDozen(number: Int): Dozen = if (number <= 12) Dozen.FirstDozen
      else if (number <= 24) Dozen.SecondDozen
      else Dozen.ThirdDozen

      def apply(number: Int): Either[String, NonZeroNumber] = if (number >= 1 && number <= 36)
        Right(
          new NonZeroNumber(
            number,
            determineColor(number),
            determineParity(number),
            determineColumn(number),
            determineNumberRange(number),
            determineDozen(number)
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
    }
    sealed abstract case class Street private (begin: NonZeroNumber) extends BetType
    object Street {
      def apply(begin: NonZeroNumber): Either[String, Street] = if ((begin.number - 1) % 3 == 0)
        Right(new Street(begin) {})
      else
        Left("You should choose number from first column")
    }
    sealed abstract case class Corner private (begin: NonZeroNumber) extends BetType
    object Corner {
      def apply(begin: NonZeroNumber): Either[String, Corner] = if ((begin.number - 1) % 3 == 2)
        Left("You should choose number from the first or the second column")
      else
        Right(new Corner(begin) {})
    }
    sealed abstract case class DoubleStreet private (begin: NonZeroNumber) extends BetType
    object DoubleStreet {
      def apply(begin: NonZeroNumber): Either[String, DoubleStreet] = if ((begin.number - 1) % 3 == 0)
        Right(new DoubleStreet(begin) {})
      else
        Left("You should choose number from first column")
    }
    sealed abstract case class Basket(zeroCompanion: NonZeroNumber) extends BetType
    object Basket {
      def apply(zeroCompanion: NonZeroNumber): Either[String, Basket] =
        if (zeroCompanion.number != 1 && zeroCompanion.number != 3)
          Left("You should choose 1 or 3")
        else Right(new Basket(zeroCompanion) {})
    }
    final case class FirstFour() extends BetType
    final case class Dozens(dozen: Dozen) extends BetType
    final case class Columns(column: Column) extends BetType
    final case class RedSnakeBet() extends BetType
    final case class ColorBet(color: Color) extends BetType
    final case class ParityBet(parity: Parity) extends BetType
    final case class RangeBet(range: NumberRange) extends BetType
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
      case nonZero @ NonZeroNumber(_, _, _, _, _, _) => ifNonZeroFunction(bet, nonZero)
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
        loseIfZero((b, n) => if (b.dozen == n.dozen) Win else Lose)(bet, generatedNum)
    }

    object ColumnsComparator extends BetComparator[Columns] {

      override def compare(bet: Columns, generatedNum: Number): GameResult =
        loseIfZero((b, n) => if (b.column == n.column) Win else Lose)(bet, generatedNum)
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
        loseIfZero((b, n) => if (b.range == n.numberRange) Win else Lose)(bet, generatedNum)
    }
  }

  def checkGameResult(
    generatedNum: Number
  )(
    player: Player
  ): GameResult = player.bet match {
    case bet @ Single(_)       => SingleComparator.compare(bet, generatedNum)
    case bet @ Split(_, _)     => SplitComparator.compare(bet, generatedNum)
    case bet @ Street(_)       => StreetComparator.compare(bet, generatedNum)
    case bet @ Corner(_)       => CornerComparator.compare(bet, generatedNum)
    case bet @ DoubleStreet(_) => DoubleStreetComparator.compare(bet, generatedNum)
    case bet @ Basket(_)       => BasketComparator.compare(bet, generatedNum)
    case bet @ FirstFour()     => FirstFourComparator.compare(bet, generatedNum)
    case bet @ Dozens(_)       => DozensComparator.compare(bet, generatedNum)
    case bet @ Columns(_)      => ColumnsComparator.compare(bet, generatedNum)
    case bet @ RedSnakeBet()   => RedSnakeBetComparator.compare(bet, generatedNum)
    case bet @ ColorBet(_)     => ColorBetComparator.compare(bet, generatedNum)
    case bet @ ParityBet(_)    => ParityBetComparator.compare(bet, generatedNum)
    case bet @ RangeBet(_)     => RangeBetComparator.compare(bet, generatedNum)
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
