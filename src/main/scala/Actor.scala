sealed trait Actor
abstract class BaseActor(id: Int, pos: Position) extends Actor
case class Explorer(id: Int, pos: Position) extends BaseActor(id, pos)
case class Wanderer(id: Int, pos: Position) extends BaseActor(id, pos)
