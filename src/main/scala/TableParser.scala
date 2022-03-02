object TableParser {
  case class CellParser(rowCnt:Int,colCnt:Int) {
    def parseCellNum(cellNum:String):(Int,Int)={
      val (word,num) = cellNum.split()
    }
  }
  class Cell(rowCnt:Int,colCnt:Int,cellNum:String,val expr:String) {
    val (row,col) = parseCellNum
  }
}
