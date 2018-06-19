sealed trait Tile

case object Wall extends Tile
case object Empty extends Tile
case object Spawn extends Tile

case class Level(lines: Seq[String]) {

  val grid = Array.ofDim[Tile](lines.length, lines(0).length)

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

  var actors: List[Actor] = List()
  var allExplorers: List[Explorer] = List()
  var explorers: List[Explorer] = List()
  var wanderers: List[Wanderer] = List()
  var player: Explorer = Explorer(Position(0, 0))

  def addActor(actor: Actor, isPlayer: Boolean = false): Unit = {
    actor match {
      case e @ Explorer (pos)  if isPlayer => player.pos.set(pos); allExplorers ::= e; actors ::= e
      case e @ Explorer(pos)  => allExplorers ::= e; actors ::= e; explorers ::= e
      case w @ Wanderer(pos)  => wanderers ::= w; actors ::= w
    }
  }

}
