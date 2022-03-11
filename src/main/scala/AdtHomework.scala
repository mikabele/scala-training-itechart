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

    def apply(number: Int): Parity = if (number % 2 == 0) Even else Odd
  }

  sealed trait Dozen
  object Dozen {
    final case object FirstDozen extends Dozen
    final case object SecondDozen extends Dozen
    final case object ThirdDozen extends Dozen

    def apply(number: Int): Dozen =
      if (number >= 1 && number <= 12) FirstDozen else if (number >= 13 && number <= 24) SecondDozen else ThirdDozen
  }

  sealed trait Column
  object Column {
    final case object FirstColumn extends Column
    final case object SecondColumn extends Column
    final case object ThirdColumn extends Column

    def apply(number: Int): Column = (number - 1) % 3 match {
      case 0 => FirstColumn
      case 1 => SecondColumn
      case 2 => ThirdColumn
    }
  }

  sealed trait NumberRange
  object NumberRange {
    final case object Low extends NumberRange
    final case object High extends NumberRange

    def apply(number: Int): NumberRange = if (number >= 1 && number <= 18) Low else High
  }

  sealed trait Number
  object Number {

    final case object Zero extends Number
    final case class NonZeroNumber(
      number: Int,
      color:  Color,
      dozen:  Dozen,
      range:  NumberRange,
      column: Column,
      parity: Parity
    ) extends Number

    private val colorMap: Map[Int, Color] = Map(
      1  -> Red,
      2  -> Black,
      3  -> Red,
      4  -> Black,
      5  -> Red,
      6  -> Black,
      7  -> Red,
      8  -> Black,
      9  -> Red,
      10 -> Black,
      11 -> Black,
      12 -> Red,
      13 -> Black,
      14 -> Red,
      15 -> Black,
      16 -> Red,
      17 -> Black,
      18 -> Red,
      19 -> Red,
      20 -> Black,
      21 -> Red,
      22 -> Black,
      23 -> Red,
      24 -> Black,
      25 -> Red,
      26 -> Black,
      27 -> Red,
      28 -> Black,
      29 -> Black,
      30 -> Red,
      31 -> Black,
      32 -> Red,
      33 -> Black,
      34 -> Red,
      35 -> Black,
      36 -> Red
    )

    def apply(number: Int): Either[String, Number] = if (number < 0 || number > 36)
      Left("Invalid 'number' arg. It should be between 0 and 36")
    else if (number == 0) Right(Zero)
    else
      Right(
        NonZeroNumber(
          number,
          colorMap(number),
          Dozen(number),
          NumberRange(number),
          Column(number),
          Parity(number)
        )
      )

  }

  sealed trait BetType
  object BetType {
    final case class Single(value: NonZeroNumber) extends BetType
    final case class Split(begin: NonZeroNumber, end: NonZeroNumber) extends BetType
    final case class Street(begin: NonZeroNumber, end: NonZeroNumber) extends BetType
    final case class Corner(begin: NonZeroNumber, end: NonZeroNumber) extends BetType
    final case class DoubleStreet(begin: NonZeroNumber, end: NonZeroNumber) extends BetType
    final case class Basket(zeroCompanion: NonZeroNumber) extends BetType
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
        loseIfZero((b, n) => if (b.begin.number == n.number || n.number == b.end.number) Win else Lose)(
          bet,
          generatedNum
        )
    }

    object StreetComparator extends BetComparator[Street] {
      override def compare(bet: Street, generatedNum: Number): GameResult =
        loseIfZero((b, n) => if (b.begin.number < n.number || n.number < b.end.number) Win else Lose)(bet, generatedNum)
    }

    object CornerComparator extends BetComparator[Corner] {
      override def compare(bet: Corner, generatedNum: Number): GameResult = loseIfZero((b, n) =>
        if (
          b.begin.number == n.number || b.begin.number + 1 == n.number || b.end.number - 1 == n.number || b.end.number == n.number
        ) Win
        else Lose
      )(bet, generatedNum)
    }

    object DoubleStreetComparator extends BetComparator[DoubleStreet] {
      override def compare(bet: DoubleStreet, generatedNum: Number): GameResult =
        loseIfZero((b, n) => if (b.begin.number < n.number || n.number < b.end.number) Win else Lose)(bet, generatedNum)
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
        loseIfZero((b, n) => if (b.range == n.range) Win else Lose)(bet, generatedNum)
    }
  }

  def checkGameResult(
    generatedNum: Number
  )(
    player: Player
  ): GameResult = player.bet match {
    case bet @ Single(_)          => SingleComparator.compare(bet, generatedNum)
    case bet @ Split(_, _)        => SplitComparator.compare(bet, generatedNum)
    case bet @ Street(_, _)       => StreetComparator.compare(bet, generatedNum)
    case bet @ Corner(_, _)       => CornerComparator.compare(bet, generatedNum)
    case bet @ DoubleStreet(_, _) => DoubleStreetComparator.compare(bet, generatedNum)
    case bet @ Basket(_)          => BasketComparator.compare(bet, generatedNum)
    case bet @ FirstFour()        => FirstFourComparator.compare(bet, generatedNum)
    case bet @ Dozens(_)          => DozensComparator.compare(bet, generatedNum)
    case bet @ Columns(_)         => ColumnsComparator.compare(bet, generatedNum)
    case bet @ RedSnakeBet()      => RedSnakeBetComparator.compare(bet, generatedNum)
    case bet @ ColorBet(_)        => ColorBetComparator.compare(bet, generatedNum)
    case bet @ ParityBet(_)       => ParityBetComparator.compare(bet, generatedNum)
    case bet @ RangeBet(_)        => RangeBetComparator.compare(bet, generatedNum)
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

  def main(args: Array[String]): Unit = {
    for {
      num <- Number(25) match {
        case Zero => Left("Generate non zero number")
        case num @ NonZeroNumber(_, _, _, _, _, _) => Right(num)
      }
      player   = Player(Single(num))
      players <- List(Player(Number(25)))
    } runGame(List(Player(Single(Number(25)))))
  }
}
