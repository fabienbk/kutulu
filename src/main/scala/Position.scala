import scala.collection.mutable.{HashSet, HashMap, Map}

object Position {
  var idCounter = 0
  val closedSet = new HashSet[Position]()
  val openSet = new HashSet[Position]()
  val cameFrom = new HashMap[Position, Position]()
  val gScore = HashMap[Position, Int]().withDefaultValue(Int.MaxValue)
  val fScore = new HashMap[Position, Int]().withDefaultValue(Int.MaxValue)
}

case class Position(var x: Int, var y: Int)(implicit val level: Level) {
  var id = 0
  def set(pos: Position) = {
    x = pos.x
    y = pos.y
    id = Position.idCounter
    Position.idCounter = Position.idCounter + 1
    this
  }

  override def toString: String = "("+x+","+y+")"

  def above : Position = level.pos(y-1)(x)
  def below : Position = level.pos(y+1)(x)
  def left : Position = level.pos(y)(x-1)
  def right : Position = level.pos(y)(x+1)

  def pathTo(target: Position, dangerCost : Boolean = false): Option[List[Position]] = genericPathTo(target, reconstructPath, dangerCost)
  def distanceTo(target: Position, dangerCost : Boolean = false): Option[Int] = genericPathTo(target, measurePath, dangerCost)

  def genericPathTo[B](target: Position,
             func : (Map[Position, Position], Position) => B,
             dangerCost: Boolean): Option[B] = {

    val closedSet = Position.closedSet
    val openSet = Position.openSet
    val cameFrom = Position.cameFrom
    val gScore = Position.gScore
    val fScore = Position.fScore

    closedSet.clear()
    openSet.clear()
    cameFrom.clear()
    gScore.clear()
    fScore.clear()

    openSet.add(this)
    gScore(this) = 0
    fScore(this) = level.dangerAt(this)

    while (!openSet.isEmpty) {
      val current = openSet.minBy(n => fScore(n)) // lowest fScore value
      if (current == target)
        return Some(func(cameFrom, current))

      openSet.remove(current)
      closedSet.add(current)

      for(neighbor <- level.neighboursAt(current).filterNot(closedSet.contains(_))) {
        if(!openSet.contains(neighbor))
          openSet.add(neighbor)

        val candidateGScore = gScore(current) + 1
        if (candidateGScore < gScore(neighbor)) { //better path
          cameFrom(neighbor) = current
          gScore(neighbor) = candidateGScore
          fScore(neighbor) = gScore(neighbor) + (if (dangerCost) level.dangerAt(neighbor) else 0)
        }
      }
    }
    return Option.empty
  }

  def reconstructPath(cameFrom: Map[Position, Position],
                      last: Position): List[Position] = {

    var total_path = List.empty[Position]
    total_path ::= last
    var current = last

    while(cameFrom.keySet.contains(current)) {
      current = cameFrom(current)
      total_path = current :: total_path
    }
    total_path
  }

  def measurePath(cameFrom: Map[Position, Position], last: Position) = {
    var pathSize = 1
    var current = last
    while(cameFrom.keySet.contains(current)) {
      current = cameFrom(current)
      pathSize += 1
    }
    pathSize
  }

}
