import math._
import scala.collection.{immutable, mutable}
import scala.util._

object Player extends App {
  trait Strategy
  case object AVOID_ENEMY extends Strategy
  case object FOLLOW_PEOPLE extends Strategy
  case object RUN_TO_PEOPLE extends Strategy
  case object GET_OUT_STALKER extends Strategy

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
        case "EXPLORER" if i == 0 => level.setActor(Explorer(id, level.pos(y)(x), plans = param1, sanity = param0), true)
        case "EXPLORER" => level.setActor(Explorer(id, level.pos(y)(x), plans = param1))
        case "WANDERER" => level.setActor(Wanderer(id, level.pos(y)(x), 0, 400, status = param1, targetId = param2))
        case "SLASHER" => level.setActor(Wanderer(id, level.pos(y)(x), 1, 800, status = param1, targetId = param2))
        case _ =>
      }
    }

    var mode : Strategy = FOLLOW_PEOPLE
    def computeNextMove(): String = {
      level.updateDangerHeatMap()

      mode = if (level.player.stalked) {
        GET_OUT_STALKER
      }
      else if (!level.explorersNearPlayer()) {
        RUN_TO_PEOPLE // safety less important
      }
      else if (level.dangerAt(level.player.pos) > 0) {
        AVOID_ENEMY // best local heuristics
      }
      else {
        FOLLOW_PEOPLE // Safety important
      }

      var nextPosition = level.player.pos

      if (mode == AVOID_ENEMY || mode == GET_OUT_STALKER) {
        val safestPosition = level.safestPlayerPosition()
        nextPosition = level.player.pathTo(safestPosition, level, true) match {
          case Some(_ :: x :: _) => x
          case _ => safestPosition
        }
      }
      else { // FOLLOW_PEOPLE or RUN_TO_PEOPLE
        // find the furthest player from any monster/spawn
        val explorer = level.explorerInSafestPosition()

        if (explorer!=null) {
          if (mode == RUN_TO_PEOPLE) return "MOVE " + explorer.pos.x + " " + explorer.pos.y
          if (explorer.pos == level.player.pos) return "WAIT"

          nextPosition = level.player.pathTo(explorer.pos, level, true) match {
            case Some(head :: x :: tail) => x
            case _ => return "WAIT"
          }
        }
        else {
          return "WAIT" // FOR THE WIN!
        }
      }

      if (nextPosition == level.player.pos)
        return "WAIT" // MOVE <x> <y> | WAIT
      else
        return "MOVE " + nextPosition.x + " " + nextPosition.y
    }

    var nextMove = computeNextMove()

    if (nextMove == "WAIT") {
      if (level.explorerCountAt(level.player.pos) > 0) {
        nextMove = "PLAN"
      }
    }

    println(nextMove + " " + mode)
  }

}

