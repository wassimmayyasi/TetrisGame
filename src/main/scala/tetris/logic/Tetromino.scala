package tetris.logic

import engine.random.RandomGenerator
import tetris.game.{Empty, IBlock, JBlock, LBlock, OBlock, SBlock, TBlock, TetrisBlock, ZBlock}

case class Tetromino(blockType: TetrisBlock, positions: Seq[Coordinates], anchor: Coordinates) {

  val nrTetrominoes: Int = 7
  def setTetromino(randomGen: RandomGenerator, columns: Int): Tetromino = {
    randomGen.randomInt(nrTetrominoes) match {
      case 0 => Tetromino(IBlock, Seq(Coordinates(columns / 2, 1), Coordinates((columns / 2) + 1, 1), Coordinates((columns / 2) - 1, 1),
                                      Coordinates((columns / 2) + 2, 1)), Coordinates(columns/2 + 1, 2).getIBlockAnchor(Coordinates(columns / 2, 1)))
      case 1 => Tetromino(JBlock, Seq(Coordinates(columns / 2, 1), Coordinates((columns / 2) - 1, 0), Coordinates((columns / 2) - 1, 1), Coordinates((columns / 2) + 1, 1)), Coordinates(columns / 2, 1))
      case 2 => Tetromino(LBlock, Seq(Coordinates(columns / 2, 1), Coordinates((columns / 2) - 1, 1), Coordinates((columns / 2) + 1, 1), Coordinates((columns / 2) + 1, 0)), Coordinates(columns / 2, 1))
      case 3 => Tetromino(OBlock, Seq(Coordinates(columns / 2, 1), Coordinates(columns / 2, 0), Coordinates((columns / 2) + 1, 1), Coordinates((columns / 2) + 1, 0)), Coordinates(columns / 2, 1))
      case 4 => Tetromino(SBlock, Seq(Coordinates(columns / 2, 1), Coordinates((columns / 2) - 1, 1), Coordinates(columns / 2, 0), Coordinates((columns / 2) + 1, 0)), Coordinates(columns / 2, 1))
      case 5 => Tetromino(TBlock, Seq(Coordinates(columns / 2, 1), Coordinates((columns / 2) - 1, 1), Coordinates(columns / 2, 0), Coordinates((columns / 2) + 1, 1)), Coordinates(columns / 2, 1))
      case 6 => Tetromino(ZBlock, Seq(Coordinates(columns / 2, 1), Coordinates((columns / 2) - 1, 0), Coordinates(columns / 2, 0), Coordinates((columns / 2) + 1, 1)), Coordinates(columns / 2, 1))
    }
  }

  def canRotateLeftOrRight(isLeft: Boolean, columns: Int, board: Board): Boolean = {
    val testRotate = if(isLeft) rotateLeftOrRight(isLeft)
    else rotateLeftOrRight(isLeft)
    for(i <- testRotate.positions.indices)
      if(testRotate.positions(i).xPos < 0 || testRotate.positions(i).xPos > columns ||
        board.currentBoard(testRotate.positions(i).yPos.toInt)(testRotate.positions(i).xPos.toInt) != Empty) return false
    true
  }

  def rotateLeftOrRight(isLeft: Boolean): Tetromino = {
    if(blockType == OBlock) return this
    var newPositions: Seq[Coordinates] = Seq()
    for(i <- positions.indices) {
      var centeredCoordinate = anchor.subtractAnchorFrom(positions(i))  //This portion is to make the blocks in the center of a grid
      if (isLeft) centeredCoordinate = centeredCoordinate.swapAndNegativeX //This was the only way I completely grasped
      else centeredCoordinate = centeredCoordinate.swapAndNegativeY
      newPositions = newPositions :+ anchor.addWithAnchor(centeredCoordinate)
    }
    this.copy(positions = newPositions)
  }

  def canMoveLeftOrRight(isLeft: Boolean, columns: Int, board: Board): Boolean = {
    for(i <- this.positions.indices) {
      if (!isLeft && (positions(i).xPos + 1 == columns + 1 || board.currentBoard(positions(i).yPos.toInt)((positions(i).xPos + 1).toInt) != Empty)) return false
      if (isLeft && (positions(i).xPos - 1 < 0 || board.currentBoard(positions(i).yPos.toInt)((positions(i).xPos - 1).toInt) != Empty)) return false
    }
    true
  }

  def moveLeftOrRight(isLeft: Boolean): Tetromino = {
    if(isLeft) this.copy(anchor = anchor.addToXAndY(-1, 0), positions = incSeq(-1, 0))
    else this.copy(anchor = anchor.addToXAndY(1, 0), positions = incSeq(1, 0))
  }

  def canMoveDown(board: Board, rows: Int): Boolean = {
    for(i <- positions.indices)
      if (positions(i).yPos + 1 == rows || board.currentBoard((positions(i).yPos + 1).toInt)(positions(i).xPos.toInt) != Empty) return false
    true
  }

  def moveDown: Tetromino = this.copy(anchor = anchor.addToXAndY(0, 1), positions = incSeq(0, 1))

  def incSeq(xInc: Int, yInc: Int): Seq[Coordinates] = {
    var incrementedSeq: Seq[Coordinates] = Seq()
    for(i <- positions.indices)
      incrementedSeq = incrementedSeq :+ positions(i).addToXAndY(xInc, yInc)
    incrementedSeq
  }
 }