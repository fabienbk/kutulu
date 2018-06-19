case class Position(var x: Int, var y: Int) {
  def set(pos: Position) = {
    x = pos.x
    y = pos.y
    this
  }
}
