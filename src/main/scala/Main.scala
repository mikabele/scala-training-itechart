import AdtHomework.BetType._
import AdtHomework.Number.NonZeroNumber
import AdtHomework.{runGame, BetType, Color, Column, Dozen, NumberRange, Parity, Player}

import scala.annotation.tailrec

object Main {

  def initNonZeroNumbers(): Either[String, List[NonZeroNumber]] = {
    @tailrec
    def generate(index: Int, res: List[NonZeroNumber]): Either[String, List[NonZeroNumber]] = {
      if (index == 36)
        Right(res)
      else
        NonZeroNumber(index) match {
          case Right(n) => generate(index + 1, res :+ n)
          case Left(e)  => Left(e)
        }
    }
    generate(1, Nil)
  }

  def main(args: Array[String]): Unit = {
    val bets = for {
      nonZeroNumbers <- initNonZeroNumbers()
      single          = Single(nonZeroNumbers.head)
      split          <- Split(nonZeroNumbers(1), nonZeroNumbers(2))
      street         <- Street(nonZeroNumbers(3))
      corner         <- Corner(nonZeroNumbers(4))
      doubleStreet   <- DoubleStreet(nonZeroNumbers(6))
      basket         <- Basket(nonZeroNumbers(2))
      firstFour       = FirstFour()
      dozens          = Dozens(Dozen.FirstDozen)
      columns         = Columns(Column.FirstColumn)
      redSnake        = RedSnakeBet()
      color           = ColorBet(Color.Red)
      parity          = ParityBet(Parity.Even)
      range           = RangeBet(NumberRange.High)
    } yield List(
      single,
      split,
      street,
      corner,
      doubleStreet,
      basket,
      firstFour,
      dozens,
      columns,
      redSnake,
      color,
      parity,
      range
    )
    bets match {
      case Right(clearBets) => runGame(clearBets.map(Player)).foreach(println)
      case Left(e)          => println(e)
    }
  }
}
