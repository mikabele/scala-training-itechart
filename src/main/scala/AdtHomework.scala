import AdtHomework.Column.Column
import AdtHomework.Dozen.Dozen
import AdtHomework.NumberRange.NumberRange

import scala.util.Random

object AdtHomework {

  // Implement ADTs for Roulette game

  // https://www.venetian.com/casino/table-games/roulette-basic-rules.html
  // https://www.mastersofgames.com/rules/roulette-rules.htm

  // Use material from adt workshop for implementation of the following data structures:

  /*
    - Number (1 - 36)
    - Color (Red or Black)
    - Bet Type (split, straight up, street, single number etc ...)
    - Player
    - GameResult
    - others if required
   */

  // Also you need to implement roulette engine methods

  final case class Player(bet: BetType)
  sealed trait GameResult
  final case object Win extends GameResult
  final case object Lose extends GameResult

  sealed trait Color
  final case object Black extends Color
  final case object Red extends Color

  sealed trait Parity
  final case object Even extends Parity
  final case object Odd extends Parity

  object Dozen extends Enumeration {
    type Dozen = Value

    val FIRST:  AdtHomework.Dozen.Value = Value(0)
    val SECOND: AdtHomework.Dozen.Value = Value(1)
    val THIRD:  AdtHomework.Dozen.Value = Value(2)
  }

  object Column extends Enumeration {
    type Column = Value
    val FIRST:  AdtHomework.Column.Value = Value(0)
    val SECOND: AdtHomework.Column.Value = Value(1)
    val THIRD:  AdtHomework.Column.Value = Value(2)
  }

  object NumberRange extends Enumeration {
    type NumberRange = Value

    val LOW:  AdtHomework.NumberRange.Value = Value(0)
    val HIGH: AdtHomework.NumberRange.Value = Value(1)
  }

  sealed trait Number
  object Number {

    final case object Zero extends Number
    final case class NonZeroNumber(number: Int, color: Color, dozen: Dozen, range: NumberRange, column: Column)
      extends Number

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

    def apply(number: Int): Either[String, Number] = number match {
      case number if number < 0 || number > 36 => Left("Invalid 'number' arg. It should be between 0 and 36")
      case number if number == 0               => Right(Zero)
      case _ =>
        Right(
          NonZeroNumber(
            number,
            colorMap(number),
            Dozen(number / 12),
            NumberRange(number / 18),
            Column((number - 1) % 3)
          )
        )
    }
  }

  sealed trait BetType
  object BetType {
    final case class Single(value: Number)
    final case class Split(begin: Number, end: Number)
    final case class Street(begin: Number, end: Number)
    final case class Corner(begin: Number, end: Number)
    final case class DoubleStreet(begin: Number, end: Number)
    final case class Basket(zeroCompanion: Number)
    final case class FirstFour()
    final case class Dozens(dozen: Dozen)
    final case class Columns(column: Column)
    final case class RedSnakeBet()
    final case class ColorBet(color: Color)
    final case class ParityBet(parity: Parity)
    final case class RangeBet(range: Range)
  }

  // Is the function below a pure function? - No, it isn't deterministic
  def generateNumber(): Either[String, Number] = {
    val randNum = Random.between(0, 37)
    Number(randNum)
  }

  def checkGameResult(generatedNum: Number)(player: Player): GameResult = {}

  def runGame(players: List[Player]): Either[String, List[GameResult]] = {
    generateNumber() match {
      case Right(num) =>
        Right(players.map(checkGameResult(num)))
      case Left(e) => Left(e)
    }
  }

  // add some tests via main or even unit tests to verify the game logic

  def main(args: Array[String]): Unit = {
    for {
      num <- generateNumber()
    } yield ()
  }
}
