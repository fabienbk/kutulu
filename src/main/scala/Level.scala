import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

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
  var dangerMapBuffer = Array.ofDim[Int](lines.length, lines(0).length)

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
      val wpos = wanderers(i).pos
      Tools.blurDanger(wpos, this)
      i += 1
    }
  }

  def setActor(actor: Actor, isPlayer: Boolean = false): Unit = {
    val actorToUpdate = actors.getOrElseUpdate(actor.id, {
      actor match {
        case e @ Explorer (id, pos)  if isPlayer => player = e
        case e @ Explorer(id, pos)  => explorers += e
        case w @ Wanderer(id, pos)  => wanderers += w
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

  def safestPlayerPosition() : Position = {
    var currBestPosition = player.pos
    val initDanger = dangerAt(player.pos)
    var currDanger = initDanger
    val above = player.pos.above
    val below = player.pos.below
    val left = player.pos.left
    val right = player.pos.right
    val dangerAbove = dangerAt(above)
    val dangerBelow = dangerAt(below)
    val dangerRight = dangerAt(right)
    val dangerLeft = dangerAt(left)

    if (gridAt(above) != Wall && currDanger >= dangerAbove) {
      currBestPosition = above
      currDanger = dangerAbove
    }
    if (gridAt(below) != Wall && currDanger >= dangerBelow) {
      currBestPosition = below
      currDanger = dangerBelow
    }
    if (gridAt(left) != Wall && currDanger >= dangerLeft) {
      currBestPosition = left
      currDanger = dangerLeft
    }
    if (gridAt(right) != Wall && currDanger >= dangerRight) {
      currBestPosition = right
      currDanger = dangerRight
    }

    currBestPosition
  }

  override def toString(): String = {
    grid.map(_.map(_.toString.charAt(0)).mkString("")).mkString("\n") + "\n" +
      "Player   : " +player.toString + "\n" +
      "Explorers: " +explorers.toString + "\n" +
      "Wanderers: " +wanderers.toString
  }

}
