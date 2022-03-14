import AdtHomework.BetType._
import AdtHomework.GameResult.{Lose, Win}
import AdtHomework.Number.NonZeroNumber
import AdtHomework._
import org.scalatest.funspec.AnyFunSpec

class AdtHomeworkTaskTest extends AnyFunSpec {

  describe("Single") {
    it("should return Win only if it equals to generated number") {
      val res = for {
        actualNum    <- NonZeroNumber(10)
        expectedNum  <- NonZeroNumber(12)
        single        = Single(actualNum)
        player        = Player(single)
        successfulRes = checkGameResult(actualNum)(player)
        wrongRes      = checkGameResult(expectedNum)(player)
      } yield successfulRes :: wrongRes :: Nil
      assert(res == Right(List(Win, Lose)))
    }
  }

  describe("Split") {
    it("should return Win only if generated num equals to first or second number") {
      val res = for {
        firstNum      <- NonZeroNumber(11)
        secondNum     <- NonZeroNumber(10)
        wrongTestNum  <- NonZeroNumber(12)
        split         <- Split(firstNum, secondNum)
        player         = Player(split)
        successfulRes1 = checkGameResult(firstNum)(player)
        successfulRes2 = checkGameResult(secondNum)(player)
        wrongRes       = checkGameResult(wrongTestNum)(player)
      } yield successfulRes1 :: successfulRes2 :: wrongRes :: Nil
      assert(res == Right(List(Win, Win, Lose)))
    }
    it("can't be created if first and second number don't touch") {
      val res = for {
        firstNum  <- NonZeroNumber(2)
        secondNum <- NonZeroNumber(8)
        split     <- Split(firstNum, secondNum)
      } yield split
      assert(res == Left("First and second number don't touch"))
    }
    it(
      "should throw an error if first number takes place in th first column and the second number less than first one by 1"
    ) {
      val res = for {
        firstNum  <- NonZeroNumber(7)
        secondNum <- NonZeroNumber(6)
        split     <- Split(firstNum, secondNum)
      } yield split
      assert(
        res == Left("You can't choose second number less than first one if first one takes place in the first column")
      )
    }
    it(
      "should throw an error if first number takes place in th third column and the second number greater than first one by 1"
    ) {
      val res = for {
        firstNum  <- NonZeroNumber(6)
        secondNum <- NonZeroNumber(7)
        split     <- Split(firstNum, secondNum)
      } yield split
      assert(
        res == Left(
          "You can't choose second number greater than first one if first one takes place in the third column"
        )
      )
    }
  }

  describe("Street") {
    it("should throw an error if number is not in first column") {
      val res = for {
        num <- NonZeroNumber(11)
        bet <- Street(num)
      } yield bet
      assert(res == Left("You should choose number from first column"))
    }
    it("should return Win only if generated number is between Street.number and Street.number+3") {
      val res = for {
        num      <- NonZeroNumber(10)
        begin     = num
        end      <- NonZeroNumber(12)
        wrongNum <- NonZeroNumber(13)
        bet      <- Street(num)
        player    = Player(bet)
        beginRes  = checkGameResult(begin)(player)
        endRes    = checkGameResult(end)(player)
        wrongRes  = checkGameResult(wrongNum)(player)
      } yield beginRes :: endRes :: wrongRes :: Nil
      assert(res == Right(List(Win, Win, Lose)))
    }
  }

  describe("Corner") {
    it("should throw an exception if number is in the third column") {
      val res = for {
        num <- NonZeroNumber(9)
        bet <- Corner(num)
      } yield bet
      assert(res == Left("You should choose number from the first or the second column"))
    }
    it("should return Win if generated num is in the corner where left-up corner is selected number") {
      val res = for {
        num         <- NonZeroNumber(8)
        bet         <- Corner(num)
        player       = Player(bet)
        successNum1  = num
        successNum2 <- NonZeroNumber(9)
        successNum3 <- NonZeroNumber(11)
        successNum4 <- NonZeroNumber(12)
        wrongNum    <- NonZeroNumber(7)
        successRes1  = checkGameResult(successNum1)(player)
        successRes2  = checkGameResult(successNum2)(player)
        successRes3  = checkGameResult(successNum3)(player)
        successRes4  = checkGameResult(successNum4)(player)
        wrongRes     = checkGameResult(wrongNum)(player)
      } yield successRes1 :: successRes2 :: successRes3 :: successRes4 :: wrongRes :: Nil
      assert(res == Right(List(Win, Win, Win, Win, Lose)))
    }
  }

  describe("Basket") {
    it("should throw an error if you didn't choose 1 or 3") {
      val res = for {
        num <- NonZeroNumber(4)
        bet <- Basket(num)
      } yield bet
      assert(res == Left("You should choose 1 or 3"))
    }
    it("should return Win if generated number ==0 or 2 or selectedNum") {
      val res = for {
        num            <- NonZeroNumber(3)
        bet            <- Basket(num)
        player          = Player(bet)
        successfulNum1  = num
        successfulNum2 <- Number(2)
        zeroNum        <- Number(0)
        wrongNum       <- Number(1)
        successfulRes1  = checkGameResult(successfulNum1)(player)
        successfulRes2  = checkGameResult(successfulNum2)(player)
        zeroRes         = checkGameResult(zeroNum)(player)
        wrongRes        = checkGameResult(wrongNum)(player)
      } yield successfulRes1 :: successfulRes2 :: zeroRes :: wrongRes :: Nil
      assert(res == Right(List(Win, Win, Win, Lose)))
    }
  }

  describe("RedSnakeBet") {
    it(
      "should return Win if generated number has Red color and value from diapason 1-5-9-12-14-16-19-23-27-30-32-34"
    ) {
      val res = for {
        successfulNum <- NonZeroNumber(5)
        wrongNum1     <- NonZeroNumber(3)
        wrongNum2     <- NonZeroNumber(2)
        snake          = RedSnakeBet()
        player         = Player(snake)
        successfulRes  = checkGameResult(successfulNum)(player)
        wrongRes1      = checkGameResult(wrongNum1)(player)
        wrongRes2      = checkGameResult(wrongNum2)(player)
      } yield successfulRes :: wrongRes1 :: wrongRes2 :: Nil
      assert(res == Right(List(Win, Lose, Lose)))
    }
  }

  describe("FirstFour") {
    it("should return Win if generated number equals to 0 or 1 or 2 or 3") {
      val res = for {
        zeroNum        <- Number(0)
        successfulNum1 <- Number(1)
        successfulNum2 <- Number(2)
        successfulNum3 <- Number(3)
        wrongNum       <- Number(6)
        bet             = FirstFour()
        player          = Player(bet)
        successfulRes1  = checkGameResult(successfulNum1)(player)
        successfulRes2  = checkGameResult(successfulNum2)(player)
        successfulRes3  = checkGameResult(successfulNum3)(player)
        zeroRes         = checkGameResult(zeroNum)(player)
        wrongRes        = checkGameResult(wrongNum)(player)
      } yield successfulRes1 :: successfulRes2 :: successfulRes3 :: zeroRes :: wrongRes :: Nil
      assert(res == Right(List(Win, Win, Win, Win, Lose)))
    }
  }

  describe("Dozens") {
    it("should return Win only if numbers have the same Dozen") {
      val res = for {
        successfulNum <- Number(8)
        wrongNum      <- Number(21)
        bet           <- Dozens(1)
        player         = Player(bet)
        successfulRes  = checkGameResult(successfulNum)(player)
        wrongRes       = checkGameResult(wrongNum)(player)
      } yield successfulRes :: wrongRes :: Nil
      assert(res == Right(List(Win, Lose)))
    }
  }

  describe("Columns") {
    it("should return Win only if numbers have the same Column") {
      val res = for {
        successfulNum <- Number(8)
        wrongNum      <- Number(21)
        bet           <- Columns(2)
        player         = Player(bet)
        successfulRes  = checkGameResult(successfulNum)(player)
        wrongRes       = checkGameResult(wrongNum)(player)
      } yield successfulRes :: wrongRes :: Nil
      assert(res == Right(List(Win, Lose)))
    }
  }

  describe("ColorBet") {
    it("should return Win only if numbers have the same Color") {
      val res = for {
        successfulNum <- Number(1)
        wrongNum      <- Number(2)
        bet            = ColorBet(Color.Red)
        player         = Player(bet)
        successfulRes  = checkGameResult(successfulNum)(player)
        wrongRes       = checkGameResult(wrongNum)(player)
      } yield successfulRes :: wrongRes :: Nil
      assert(res == Right(List(Win, Lose)))
    }
  }

  describe("ParityBet") {
    it("should return Win only if numbers have the same Parity") {
      val res = for {
        successfulNum <- Number(1)
        wrongNum      <- Number(2)
        bet            = ParityBet(Parity.Odd)
        player         = Player(bet)
        successfulRes  = checkGameResult(successfulNum)(player)
        wrongRes       = checkGameResult(wrongNum)(player)
      } yield successfulRes :: wrongRes :: Nil
      assert(res == Right(List(Win, Lose)))
    }
  }
  describe("RangeBet") {
    it("should return Win only if numbers have the same Range") {
      val res = for {
        successfulNum <- Number(19)
        wrongNum      <- Number(2)
        bet           <- RangeBet(2)
        player         = Player(bet)
        successfulRes  = checkGameResult(successfulNum)(player)
        wrongRes       = checkGameResult(wrongNum)(player)
      } yield successfulRes :: wrongRes :: Nil
      assert(res == Right(List(Win, Lose)))
    }
  }

  describe("Number") {
    it("should create Number or NonZeroNumber if argument is between 0(1) and 36") {
      assert(Number(40) == Left("Invalid 'number' arg. It should be between 0 and 36"))
      assert(NonZeroNumber(-2) == Left("Invalid 'number' arg. It should be between 1 and 36"))
    }
  }
}
