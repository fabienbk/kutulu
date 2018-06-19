sealed trait Actor {
  var pos: Position
  var id: Int
}

abstract class BaseActor(_id: Int, _pos: Position) extends Actor {
  var pos = _pos
  var id = _id
}

case class Explorer(_id: Int, _pos: Position) extends BaseActor(_id, _pos) {
}

case class Wanderer(_id: Int, _pos: Position) extends BaseActor(_id, _pos) {
}
