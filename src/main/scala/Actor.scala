
sealed trait Actor {
  var pos: Position
  var id: Int
}

abstract class BaseActor(_id: Int, _pos: Position) extends Actor {
  var pos = _pos
  var id = _id

  def distanceTo(targetActor: BaseActor, level: Level, dangerCost: Boolean = false) = pos.distanceTo(targetActor.pos, dangerCost)
  def pathTo(position: Position, level: Level, dangerCost: Boolean = false) = pos.pathingTo(position, dangerCost)
}

case class Explorer(_id: Int, _pos: Position, plans: Int = 2, sanity: Int = 100) extends BaseActor(_id, _pos) {
  var stalked = false

  def dangerScore(level: Level): Int = {
    val danger = level.dangerAt(_pos)
    val normalizedDanger = if (danger > 5) 5 else danger
    normalizedDanger + Math.abs(level.player.pos.x - pos.x) + Math.abs(level.player.pos.y - pos.y)
  }
}

case class Wanderer(_id: Int,
                     _pos: Position,
                    kind: Int = 0,
                    danger: Int = 400,
                    status: Int = Wanderer.WANDERING,
                    targetId: Int = -1) extends BaseActor(_id, _pos) {
}
object Wanderer {
  var SPAWNING : Int = 0
  var WANDERING : Int  = 1
  var STALKING : Int = 2
  var RUSHING : Int = 3
  var STUNNED : Int = 4
}

