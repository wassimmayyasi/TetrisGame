package tetris.logic

import tetris.game.{Empty, TetrisBlock}

case class Board(currentBoard: Seq[Seq[TetrisBlock]]) {

  def settleBlocks(tetromino: Tetromino): Board = {
    var newBoard: Seq[Seq[TetrisBlock]] = currentBoard
    for(i <- tetromino.positions.indices)
      newBoard = newBoard.updated(tetromino.positions(i).yPos.toInt, newBoard(tetromino.positions(i).yPos.toInt)
                         .updated(tetromino.positions(i).xPos.toInt, tetromino.blockType))
    this.copy(currentBoard = newBoard)
  }

  def fixBoard(changingBoard: Seq[Seq[TetrisBlock]], row: Int): Seq[Seq[TetrisBlock]] = {
    var newBoard: Seq[Seq[TetrisBlock]] = changingBoard.updated(0,Seq.fill(changingBoard.size - 1)(Empty))
    for (i <- 1 to row)
      newBoard = newBoard.updated(i,changingBoard(i - 1))
    newBoard
  }

  def processFullLines: Board = {
    var newBoard: Seq[Seq[TetrisBlock]] = currentBoard
    for (i <- newBoard.indices) {
      val thisRow: Seq[TetrisBlock] = newBoard(i)
      if (!thisRow.contains(Empty)) newBoard = fixBoard(newBoard, i)
    }
    this.copy(currentBoard = newBoard)
  }
}