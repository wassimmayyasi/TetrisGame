// This file should not be present in skeleton, used in creating tests

package test.tetris

import java.awt.event
import java.awt.event.KeyEvent._

import engine.GameBase
import engine.graphics.{Color, Point, Rectangle}
import engine.random.ListRandomGen
import processing.core.{PApplet, PConstants}
import processing.event.KeyEvent
import snake.game.SnakeGame.{FramesPerSecond, HeightCellInPixels, WidthCellInPixels}
import tetris.game.{Empty, IBlock, JBlock, LBlock, OBlock, SBlock, TBlock, TetrisBlock, ZBlock}
import tetris.logic._
import test.tetris.TetrisRecord._

object TetrisGameRecorder {

  val FramesPerSecond: Int = 5
  val WidthCellInPixels: Int = 15
  val HeightCellInPixels: Int = WidthCellInPixels

  def main(args:Array[String]): Unit = {
    PApplet.main("test.tetris.TetrisGameRecorder")
  }

}

class TetrisGameRecorder extends GameBase {

  val random = new ListRandomGen(List(5,0) )
  val init : GridDisplay = gridString(
    """....
      |....
      |....
      |...O
      |Z.ZZ
      |Z.ZZ
      |Z.ZZ
      |Z.ZZ
      |.ZZZ""")



  var gameLogic = new TetrisLogic(random , 4,6)// ,init.grid.map(_.map(_.gridType)) )
  val updateTimer = new UpdateTimer(FramesPerSecond)
  val widthInPixels: Int = WidthCellInPixels * gameLogic.nrColumns
  val heightInPixels: Int = HeightCellInPixels * gameLogic.nrRows
  val screenArea: Rectangle = Rectangle(Point(0, 0), widthInPixels, heightInPixels)
  var actions : Seq[TetrisAction] = List()

  // this function is wrongly named draw by processing (is called on each update)
  override def draw(): Unit = {
    updateState()
    drawGrid()
    if (gameLogic.isGameOver) drawGameOverScreen()
  }

  def drawGameOverScreen(): Unit = {
    setFillColor(Color.Red)
    drawTextCentered("GAME OVER!", 20, screenArea.center)
  }

  def getScreen :GameDisplay = {
    if(!gameLogic.isGameOver) {
      GridDisplay(
        for(y <- 0 until  gameLogic.nrRows)
          yield for(x <- 0 until gameLogic.nrColumns)
            yield TetrisGridTypeWrapper(gameLogic.getBlockAt(x,y)))
    } else {
      GameOverDisplay()
    }
  }

  def drawGrid(): Unit = {
    val widthPerCell = screenArea.width / gameLogic.nrColumns
    val heightPerCell = screenArea.height / gameLogic.nrRows

    for (y <- 0 until gameLogic.nrRows;
         x <- 0 until gameLogic.nrColumns) {
      drawCell(getCell(x, y), gameLogic.getBlockAt(x,y))
    }

    def getCell(colIndex: Int, rowIndex: Int): Rectangle = {
      val leftUp = Point(screenArea.left + colIndex * widthPerCell,
        screenArea.top + rowIndex * heightPerCell)
      Rectangle(leftUp, widthPerCell, heightPerCell)
    }


    def drawCell(area: Rectangle, tetrisColor: TetrisBlock): Unit = {
      val color = tetrisColorToRGB(tetrisColor)
      setFillColor(color)
      drawRectangle(area)
    }

  }

  def addAction(action : TetrisAction) = {
    actions = actions :+ action
  }

  def dumpInfo() : Unit = {
    val testFrame = TestFrame(FrameInput(random.last, actions), getScreen )
    println(testFrame + ",")
    actions = List()
  }

  /** Method that calls handlers for different key press events.
    * You may add extra functionality for other keys here.
    * See [[event.KeyEvent]] for all defined keycodes.
    *
    * @param event The key press event to handle
    */
  override def keyPressed(event: KeyEvent): Unit = {

    event.getKeyCode match {
      case VK_A  => { gameLogic.rotateLeft(); addAction(RotateLeft) }
      case VK_S  => { gameLogic.rotateRight(); addAction(RotateRight) }
      case VK_UP    => { gameLogic.rotateRight(); addAction(RotateRight) }
      case VK_DOWN  => { gameLogic.moveDown(); addAction(Down) }
      case VK_LEFT  => { gameLogic.moveLeft(); addAction(Left) }
      case VK_RIGHT => { gameLogic.moveRight(); addAction(Right) }
      case VK_SPACE => { gameLogic.doHardDrop(); addAction(Drop) }
      case VK_D => {dumpInfo()}
      case _ => ()
    }
    dumpInfo()

  }

  override def settings(): Unit = {
    pixelDensity(displayDensity())
    size(widthInPixels, heightInPixels, PConstants.P2D)
  }

  override def setup(): Unit = {
    // Fonts are loaded lazily, so when we call text()
    // for the first time, there is significant lag.
    // This prevents it from happening during gameplay.
    text("", 0, 0)
    // This should be called last, since the game
    // clock is officially ticking at this point
    updateTimer.init()
  }


  def updateState(): Unit = {
   /* if (updateTimer.timeForNextFrame()) {
      gameLogic.down()
      updateTimer.advanceFrame()
    }*/
  }

  def tetrisColorToRGB(color : TetrisBlock) : Color =
    color match {
      case IBlock =>  Color(173,216,230)
      case OBlock =>  Color(255,255,0)
      case LBlock => Color(255,165,0)
      case JBlock => Color(0,0,255)
      case SBlock => Color(0,255,0)
      case Empty => Color(0,0,0)
      case TBlock => Color(128,0,128)
      case ZBlock => Color(255,0,0)
    }

}


