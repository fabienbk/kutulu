sealed trait Actor {
  var pos: Position
  var id: Int
}

abstract class BaseActor(_id: Int, _pos: Position) extends Actor {
  var pos = _pos
  var id = _id

  def distanceTo(targetActor: BaseActor, level: Level, dangerCost: Boolean = false) = pos.distanceTo(targetActor.pos, dangerCost)
  def pathTo(position: Position, level: Level, dangerCost: Boolean = false) = pos.pathTo(position, dangerCost)
}

case class Explorer(_id: Int, _pos: Position) extends BaseActor(_id, _pos) {

  def dangerScore(level: Level): Int = {
    level.dangerAt(_pos)
  }
}

case class Wanderer(_id: Int, _pos: Position) extends BaseActor(_id, _pos) {
}
