import scala.collection.mutable.{HashSet, HashMap, Set, Map}

case class Position(var x: Int, var y: Int) {
  def set(pos: Position) = {
    x = pos.x
    y = pos.y
    this
  }

  def above : Position = Position(x,y-1)
  def below : Position = Position(x,y+1)
  def left : Position = Position(x-1,y)
  def right : Position = Position(x+1,y)

  def pathTo(target: Position, level : Level, dangerCost : Boolean = false): Option[List[Position]] = genericPathTo(target, level, reconstructPath, dangerCost)
  def distanceTo(target: Position, level : Level, dangerCost : Boolean = false): Option[Int] = genericPathTo(target, level, measurePath, dangerCost)

  def genericPathTo[B](target: Position,
             level : Level,
             func : (Map[Position, Position], Position) => B,
             dangerCost: Boolean): Option[B] = {

    val closedSet = new HashSet[Position]()
    val openSet = new HashSet[Position]()
    openSet.add(this)
    val cameFrom = new HashMap[Position, Position]()
    val gScore = HashMap[Position, Int]().withDefaultValue(Int.MaxValue)
    gScore(this) = 0
    val fScore = new HashMap[Position, Int]().withDefaultValue(Int.MaxValue)
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
