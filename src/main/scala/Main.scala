import java.io.{File, FileWriter}

object Main {

  def writeFile(filename: String, line: String): Unit = {
    val file = new File(filename)
    val fw   = new FileWriter(file)
    fw.write(line)
    fw.close()
  }

  def main(args: Array[String]): Unit = {
    val res = Solver.process(args.mkString(" "))
    if (args.contains("--output")) writeFile(args(args.indexOf("--output") + 1), res)
    else println(res)
  }
}
