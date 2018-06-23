import math._
import scala.collection.{immutable, mutable}
import scala.util._

object Player extends App {
  trait Strategy
  case object AVOID_ENEMY
  case object FOLLOW_PEOPLE

  val width = readInt
  val height = readInt

  val level = new Level(for(i <- 0 until height) yield readLine)

  // sanitylosslonely: how much sanity you lose every turn when alone, always 3 until wood 1
  // sanitylossgroup: how much sanity you lose every turn when near another player, always 1 until wood 1
  // wandererspawntime: how many turns the wanderer take to spawn, always 3 until wood 1
  // wandererlifetime: how many turns the wanderer is on map after spawning, always 40 until wood 1
  val Array(sanitylosslonely, sanitylossgroup, wandererspawntime, wandererlifetime) = for(i <- readLine split " ") yield i.toInt

  while(true) {
    val entitycount = readInt // the first given entity corresponds to your explorer

    level.wanderers.clear()
    level.explorers.clear()
    level.actors.clear()

    for(i <- 0 until entitycount) {
      val Array(entitytype, _id, _x, _y, _param0, _param1, _param2) = readLine split " "
      val id = _id.toInt
      val x = _x.toInt
      val y = _y.toInt
      val param0 = _param0.toInt
      val param1 = _param1.toInt
      val param2 = _param2.toInt

      entitytype match {
        case "EXPLORER" if i == 0 => level.setActor(Explorer(id, level.pos(y)(x)), true)
        case "EXPLORER" => level.setActor(Explorer(id, level.pos(y)(x)))
        case "WANDERER" => level.setActor(Wanderer(id, level.pos(y)(x)))
        case _ =>
      }
    }

    def computeNextMove(): String = {
      var budget: Long = 40
      val t0 = System.currentTimeMillis()

      level.updateDangerHeatMap()

      val t1 = System.currentTimeMillis()
      budget -= (t1 - t0)
      if (budget <= 20) return "WAIT"

      val mode = if (level.dangerAt(level.player.pos) > 0) AVOID_ENEMY else FOLLOW_PEOPLE

      val t2 = System.currentTimeMillis()
      budget -= (t2 - t1)
      if (budget <= 20) return "WAIT"

      var position = level.player.pos
      if (mode == AVOID_ENEMY) {
        position = level.safestPlayerPosition()
      }
      else {
        // find the furthest player from any monster/spawn
        val explorer = level.explorerInSafestPosition()
        if (explorer!=null) {
          val t3 = System.currentTimeMillis()
          budget -= (t3 - t2)

          if (explorer.pos == level.player.pos) return "WAIT"

          position = level.player.pathTo(explorer.pos, level, true) match {
            case Some(head :: x :: tail) => x
            case _ => return "WAIT"
          }
        }
        else {
          return "WAIT"
        }
      }

      if (position == level.player.pos)
        return "WAIT" // MOVE <x> <y> | WAIT
      else
        return "MOVE " + position.x + " " + position.y
    }

    println(computeNextMove())
  }

}

