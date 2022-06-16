package tetris.logic

case class Coordinates(xPos: Float, yPos: Float) {
  def addToXAndY(xAdd: Float, yAdd: Float): Coordinates = Coordinates(xPos + xAdd, yPos + yAdd)
  def swapAndNegativeX: Coordinates = Coordinates(yPos, xPos * -1)
  def swapAndNegativeY: Coordinates = Coordinates(yPos * -1, xPos)
  def subtractAnchorFrom(coordinates: Coordinates): Coordinates =
    Coordinates(coordinates.xPos - xPos, coordinates.yPos - yPos)
  def addWithAnchor(coordinates: Coordinates): Coordinates =
    Coordinates(coordinates.xPos + xPos, coordinates.yPos + yPos)
  def getIBlockAnchor(coordinates: Coordinates): Coordinates =
    Coordinates((coordinates.xPos + xPos)/2, (coordinates.yPos + yPos)/2)
}