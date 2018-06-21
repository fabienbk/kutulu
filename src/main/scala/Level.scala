import Tools.blur

import scala.collection.mutable
import scala.collection.mutable.HashMap

sealed trait Tile
case object Wall extends Tile
case object Empty extends Tile
case object Spawn extends Tile

case class Level(lines: Seq[String]) {

  val grid = Array.ofDim[Tile](lines.length, lines(0).length)
  val neighbours = Array.ofDim[List[Position]](lines.length, lines(0).length)
  val gridPath = Array.ofDim[Boolean](lines.length, lines(0).length)
  var dangerMap = Array.ofDim[Int](lines.length, lines(0).length)
  var dangerMapBuffer = Array.ofDim[Int](lines.length, lines(0).length)

  var actors: mutable.HashMap[Int, Actor] = HashMap.empty[Int, Actor];
  var allExplorers: List[Explorer] = List()
  var explorers: List[Explorer] = List()
  var wanderers: List[Wanderer] = List()
  var player: Explorer = null

  private def init = {
    for (y <- 0 until lines.length) {
      for (x <- 0 until lines(y).length) {
        grid(y)(x) = lines(y)(x) match {
          case '#' => Wall
          case '.' => Empty
          case 'w' => Spawn
        }
        neighbours(y)(x) = List.empty
      }
    }
    // static neighbours
    for (y <- 1 until lines.length - 1) {
      for (x <- 1 until lines(y).length - 1) {
        val p = Position(x,y)
        for (op <- Seq(p.above, p.below, p.left, p.right))
          if (gridAt(op) != Wall) neighbours(y)(x) ::= op
      }
    }
  }
  init

  def apply(x: Int, y: Int): Tile = grid(y)(x)
  def width = grid(0).length
  def height = grid.length
  def gridAt(pos: Position): Tile = grid(pos.y)(pos.x)
  def dangerAt(pos: Position): Int = dangerMap(pos.y)(pos.x)
  def neighboursAt(pos: Position): List[Position] = neighbours(pos.y)(pos.x)

  def updateDangerHeatMap() = {
    for(y <- 0 until height) {
      for (x <- 0 until width) {
        dangerMap(y)(x) = 0
      }
    }
    for(wpos <- wanderers.map(_.pos))
      dangerMap(wpos.y)(wpos.x) += 400

    for(i <- 1 to 3) {
      blur(dangerMap, dangerMapBuffer)
      val oldDangerMap = dangerMap
      dangerMap = dangerMapBuffer
      dangerMapBuffer = oldDangerMap
    }
  }


  def setActor(actor: Actor, isPlayer: Boolean = false): Unit = {
    val actorToUpdate = actors.getOrElseUpdate(actor.id, {
      actor match {
        case e @ Explorer (id, pos)  if isPlayer => allExplorers ::= e; player = e
        case e @ Explorer(id, pos)  => allExplorers ::= e; explorers ::= e
        case w @ Wanderer(id, pos)  => wanderers ::= w
      }
      actor
    })

    // Update actor data
    actorToUpdate.pos = actor.pos
  }

  def explorerInSafestPosition() : Explorer = explorers.minBy(_.dangerScore(this))

  def closestWandererToPlayer() : Option[(Wanderer, Int)] = {
    if (wanderers.isEmpty) {
      return Option.empty
    }
    return Some(wanderers
      .map(w => (w, player.distanceTo(w, this, false).getOrElse(Int.MaxValue)))
      .minBy(_._2))
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
    var debug = ""

    if (gridAt(above) != Wall && currDanger >= dangerAbove) {
      currBestPosition = above
      currDanger = dangerAbove
      debug+=s"U=$dangerAbove "
    }
    if (gridAt(below) != Wall && currDanger >= dangerBelow) {
      currBestPosition = below
      currDanger = dangerBelow
      debug+=s"D=$dangerBelow "
    }
    if (gridAt(left) != Wall && currDanger >= dangerLeft) {
      currBestPosition = left
      currDanger = dangerLeft
      debug+=s"L=$dangerLeft "
    }
    if (gridAt(right) != Wall && currDanger >= dangerRight) {
      currBestPosition = right
      currDanger = dangerRight
      debug+=s"R=$dangerRight "
    }

    Console.err.println(s"danger=$initDanger $debug => from ${player.pos} to $currBestPosition")

    currBestPosition
  }

  override def toString(): String = {
    grid.map(_.map(_.toString.charAt(0)).mkString("")).mkString("\n") + "\n" +
      "Player   : " +player.toString + "\n" +
      "Explorers: " +explorers.toString + "\n" +
      "Wanderers: " +wanderers.toString
  }

}
