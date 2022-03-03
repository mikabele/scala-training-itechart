import scala.util.{Failure, Success}

object Main extends App {
  TableParser.process("--input input.txt --output output.txt") match {
    case Failure(exception) => exception.printStackTrace()
    case _                  => println("Yuhu")
  }
}
