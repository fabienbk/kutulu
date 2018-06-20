import Tools.blur

import scala.collection.mutable.HashMap

sealed trait Tile
case object Wall extends Tile
case object Empty extends Tile
case object Spawn extends Tile

case class Level(lines: Seq[String]) {

  def updateDangerHeatMap() = {
    for(y <- 0 until height) {
      for (x <- 0 until width) {
        dangerMap(y)(x) = grid(y)(x) match {
          case Spawn => 100
          case _ => 0
        }
      }
    }
    for(wpos <- wanderers.map(_.pos))
      dangerMap(wpos.y)(wpos.x) += 100

    for(i <- 1 to 2) {
      blur(dangerMap, dangerMapBuffer)
      val oldDangerMap = dangerMap
      dangerMap = dangerMapBuffer
      dangerMapBuffer = oldDangerMap
    }
    println(dangerMap.map(_.mkString(","))mkString("\n"))
  }


  val grid = Array.ofDim[Tile](lines.length, lines(0).length)

  var dangerMap = Array.ofDim[Int](lines.length, lines(0).length)
  var dangerMapBuffer = Array.ofDim[Int](lines.length, lines(0).length)

  for(y <- 0 to lines.length - 1)
    for(x <- 0 to lines(y).length - 1)
      grid(y)(x) = lines(y)(x) match {
        case '#' => Wall
        case '.' => Empty
        case 'w' => Spawn
      }

  def apply(x: Int, y: Int): Tile = grid(y)(x)
  def width = grid(0).length
  def height = grid.length

  var actors: HashMap[Int, Actor] = HashMap.empty[Int, Actor];
  var allExplorers: List[Explorer] = List()
  var explorers: List[Explorer] = List()
  var wanderers: List[Wanderer] = List()
  var player: Explorer = null

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

  override def toString(): String = {
    grid.map(_.map(_.toString.charAt(0)).mkString("")).mkString("\n") + "\n" +
      "Player   : " +player.toString + "\n" +
      "Explorers: " +explorers.toString + "\n" +
      "Wanderers: " +wanderers.toString
  }

}
