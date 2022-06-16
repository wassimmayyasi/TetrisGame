package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.game._
import tetris.logic.TetrisLogic._

class TetrisLogic(val randomGen: RandomGenerator,
                  val nrColumns: Int,
                  val nrRows: Int,
                  val initialBoard: Seq[Seq[TetrisBlock]]) {

  def this(random: RandomGenerator, nrColumns: Int, nrRows: Int) =
    this(random, nrColumns, nrRows, makeEmptyBoard(nrColumns, nrRows))

  def this() =
    this(new ScalaRandomGen(), DefaultWidth, DefaultHeight, makeEmptyBoard(DefaultWidth, DefaultHeight))

  var currentFrame = GameFrame(nrRows, nrColumns - 1, randomGen,
                               tetromino = Tetromino(Empty, Seq[Coordinates](), Coordinates(-1, -1)),
                               board = Board(initialBoard), isGameOver = false)

  def rotateLeft(): Unit = currentFrame = currentFrame.rotateLeftOrRight(true)

  def rotateRight(): Unit = currentFrame = currentFrame.rotateLeftOrRight(false)

  def moveLeft(): Unit = currentFrame = currentFrame.moveLeftOrRight(true)

  def moveRight(): Unit = currentFrame = currentFrame.moveLeftOrRight(false)

  def moveDown(): Unit = currentFrame = currentFrame.moveDownAndSpawn

  def doHardDrop(): Unit = currentFrame = currentFrame.doHardDrop()

  def isGameOver: Boolean = currentFrame.isGameOver

  def getBlockAt(x: Int, y: Int): TetrisBlock = currentFrame.getBlocksLocation(x, y)
}
object TetrisLogic {

  def makeEmptyBoard(nrColumns: Int, nrRows: Int): Seq[Seq[TetrisBlock]] = {
    val emptyLine = Seq.fill(nrColumns)(Empty)
    Seq.fill(nrRows)(emptyLine)
  }

  val DefaultWidth: Int = 10
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines
  def apply() = new TetrisLogic(new ScalaRandomGen(),
                                DefaultWidth,
                                DefaultHeight,
                                makeEmptyBoard(DefaultWidth, DefaultHeight))
}