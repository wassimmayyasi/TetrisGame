package tetris.logic

import engine.random.RandomGenerator
import tetris.game.{Empty, TetrisBlock}

case class GameFrame(rows: Int, columns: Int, randomGen: RandomGenerator,
                     var tetromino: Tetromino, var board: Board, isGameOver: Boolean) {
  tetromino = tetromino.setTetromino(randomGen, columns)

  def getBlocksLocation(x: Int, y: Int): TetrisBlock = {
    for(i <- tetromino.positions.indices) if(tetromino.positions(i).xPos.toInt == x &&
                                 tetromino.positions(i).yPos.toInt == y) return tetromino.blockType
    board.currentBoard(y)(x)
  }

  def moveDownAndSpawn: GameFrame = {
    if (isGameOver) return this
    if (tetromino.canMoveDown(board, rows)) {
      tetromino = tetromino.moveDown
      this
    } else {
      board = board.settleBlocks(tetromino)
      board = board.processFullLines
      placeBlockIfPossible
    }
  }

  def moveLeftOrRight(isLeft: Boolean): GameFrame = {
    if(isGameOver) return this
    if(tetromino.canMoveLeftOrRight(isLeft, columns, board)) tetromino = tetromino.moveLeftOrRight(isLeft)
    this
  }

  def rotateLeftOrRight(isLeft: Boolean): GameFrame = {
    if(isLeft && tetromino.canRotateLeftOrRight(isLeft, columns, board)) tetromino = tetromino.rotateLeftOrRight(isLeft)
    if(!isLeft && tetromino.canRotateLeftOrRight(isLeft, columns, board)) tetromino = tetromino.rotateLeftOrRight(isLeft)
    this
  }

  def doHardDrop(): GameFrame = {
    while(tetromino.canMoveDown(board, rows))
      tetromino = tetromino.moveDown
    moveDownAndSpawn //to spawn
  }

  def canPlaceBlock(testTetromino: Tetromino): Boolean = {
    for(i <- testTetromino.positions.indices)
      if (board.currentBoard(testTetromino.positions(i).yPos.toInt)(testTetromino.positions(i).xPos.toInt) != Empty)
        return false
    true
  }

  def placeBlockIfPossible: GameFrame = {
    var testTetromino = Tetromino(Empty, Seq[Coordinates](), Coordinates(-1,-1))
    testTetromino = testTetromino.setTetromino(randomGen, columns)
    if (!canPlaceBlock(testTetromino)) return this.copy(isGameOver = true)
    else tetromino = testTetromino
    this
  }
}