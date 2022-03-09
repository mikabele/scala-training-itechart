package CodeWars

object Sudoku {
  private val correctArr = Array(1, 2, 3, 4, 5, 6, 7, 8, 9)

  private def checkRow(row: Array[Int]): Boolean = {
    row.sorted.sameElements(correctArr)
  }

  def isValid(board: Array[Array[Int]]): Boolean = {
    val groupedArr      = board.map(_.grouped(3).toArray)
    val firstColBlocks  = groupedArr.flatMap(_(0)).grouped(9).toArray
    val secondColBlocks = groupedArr.flatMap(_(1)).grouped(9).toArray
    val thirdColBlocks  = groupedArr.flatMap(_(2)).grouped(9).toArray
    board.forall(checkRow) && board.transpose
      .forall(checkRow) && firstColBlocks.forall(checkRow) && secondColBlocks.forall(checkRow) && thirdColBlocks
      .forall(checkRow)
  }
}
