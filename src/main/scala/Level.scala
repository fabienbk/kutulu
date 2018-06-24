import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}

sealed trait Tile
case object Wall extends Tile
case object Empty extends Tile
case object Spawn extends Tile
case object Shelter extends Tile

case class Level(lines: Seq[String]) {

  val pos = Array.ofDim[Position](lines.length, lines(0).length)
  val grid = Array.ofDim[Tile](lines.length, lines(0).length)

  val neighbours = Array.ofDim[ArrayBuffer[Position]](lines.length, lines(0).length)
  val gridPath = Array.ofDim[Boolean](lines.length, lines(0).length)
  var dangerMap = Array.ofDim[Int](lines.length, lines(0).length)

  val closedSet = new HashSet[Position]()
  var distanceMap = Array.ofDim[Int](lines.length, lines(0).length)

  var actors: mutable.HashMap[Int, Actor] = HashMap.empty[Int, Actor];
  var explorers: ArrayBuffer[Explorer] = new ArrayBuffer()
  var wanderers: ArrayBuffer[Wanderer] = new ArrayBuffer()
  var player: Explorer = null

  private def init = {
    for (y <- 0 until lines.length) {
      for (x <- 0 until lines(y).length) {
        grid(y)(x) = lines(y)(x) match {
          case '#' => Wall
          case '.' => Empty
          case 'w' => Spawn
          case 'U' => Shelter
        }
        neighbours(y)(x) = new ArrayBuffer(4)
        pos(y)(x) = Position(x,y)(this)

      }
    }

    // static neighbours
    for (y <- 1 until lines.length - 1) {
      for (x <- 1 until lines(y).length - 1) {
        val p = pos(y)(x)
        for (op <- Seq(p.above, p.below, p.left, p.right))
          if (gridAt(op) != Wall) neighbours(y)(x) += op
      }
    }


    Console.err.println("Computing distance cache...")
    val t1 = System.currentTimeMillis()
    for (y <- 1 until lines.length - 1) {
      for (x <- 1 until lines(y).length - 1) {

        var pstart = pos(y)(x)

        if (gridAt(pstart) != Wall) {
          for (ty <- 1 until lines.length - 1) {
            for (tx <- 1 until lines(y).length - 1) {
              var pend = pos(ty)(tx)
              if (gridAt(pend) != Wall && pstart != pend) {
                val path = pstart.pathingTo(pend).getOrElse(Nil)
                pstart.pathTo(pend.id) = path
              }
            }
          }
        }
      }
    }
    val t2 = System.currentTimeMillis()
    Console.err.println("Computed in " + (t2-t1))


    System.gc()
  }
  init

  def apply(x: Int, y: Int): Tile = grid(y)(x)
  def width = grid(0).length
  def height = grid.length
  def gridAt(pos: Position): Tile = grid(pos.y)(pos.x)
  def dangerAt(pos: Position): Int = dangerMap(pos.y)(pos.x)
  def neighboursAt(pos: Position): ArrayBuffer[Position] = neighbours(pos.y)(pos.x)

  def updateDangerHeatMap() = {
    var x = 1
    var y = 1
    while(y < height - 1) {
      x = 0
      while (x < width - 1) {
        dangerMap(y)(x) = 0
        x += 1
      }
      y += 1
    }

    var i = 0
    while (i < wanderers.length) {
      val wanderer = wanderers(i)
      val wpos = wanderer.pos
      var danger = wanderer.danger

      if (wanderer.kind == 1) danger = 40
      else if (wanderer.status == Wanderer.SPAWNING) danger = 10
      Tools.blurDanger(wpos, this, danger)

      if (wanderer.kind == 1 &&
        (wanderer.status == Wanderer.STALKING || wanderer.status == Wanderer.RUSHING) &&
        wanderer.targetId != -1) {

        if (wanderer.targetId == player.id)
          player.stalked = true

        Tools.markLoS(wanderer.pos, player.pos, this, 800)
      }

      i += 1
    }
  }

  def setActor(actor: Actor, isPlayer: Boolean = false): Unit = {
    val actorToUpdate = actors.getOrElseUpdate(actor.id, {
      actor match {
        case e @ Explorer (id, pos, plans, sanity)  if isPlayer => player = e
        case e @ Explorer(id, pos, plans, sanity)  => explorers += e
        case w @ Wanderer(id, pos, kind, d, status, target)  => wanderers += w
      }
      actor
    })

    // Update actor data
    actorToUpdate.pos = pos(actor.pos.y)(actor.pos.x)
  }

  def explorerInSafestPosition() : Explorer = {
    if (explorers.isEmpty) return null
    return explorers.minBy(_.dangerScore(this))
  }

  def closestWandererToPlayer() : Int = {
    if (wanderers.isEmpty) return Int.MaxValue
    var i = 0;
    var minDist = Int.MaxValue
    while(i < wanderers.length) {
      var w = wanderers(i)
      var dist = player.distanceTo(w, this, false).getOrElse(Int.MaxValue)
      if (minDist > dist) minDist = dist
    }
    return minDist
  }

  def recur(position: Position, depth: Int, visit: (Position, Int) => Unit, distanceFromStart : Int = 0): Unit = {
    if (depth == 0) return
    closedSet.add(position)
    visit(position, distanceFromStart)
    var n = neighboursAt(position)
    var i = 0
    while (i < n.length) {
      if (!closedSet.contains(n(i))) recur(n(i), depth - 1, visit, distanceFromStart + 1)
      i += 1
    }
  }
  def explore(position: Position, depth : Int, visit: (Position, Int) => Unit) : Unit = {
    closedSet.clear()
    recur(position, depth, visit, 0)
  }

  def safestPlayerPosition() : Position = {
    var minDanger : Int = Int.MaxValue
    var safestPos : Position = null
    explore(player.pos, 3, (pos,_) => {
      val d = dangerAt(pos)
      if (d < minDanger) {
        minDanger = d
        safestPos = pos
      }
    })

    safestPos
  }

  def explorersNearPlayer() : Boolean = {
    var i = 0
    while (i < explorers.length) {
      val e = explorers(i)
      if (e.pos.manhattan2(player.pos)) return true
      i += 1
    }
    return false
  }

  def explorerCountAt(pos: Position) = explorers.count(_.pos == pos)


  override def toString(): String = {
    grid.map(_.map(_.toString.charAt(0)).mkString("")).mkString("\n") + "\n" +
      "Player   : " +player.toString + "\n" +
      "Explorers: " +explorers.toString + "\n" +
      "Wanderers: " +explorers.toString
  }

}
