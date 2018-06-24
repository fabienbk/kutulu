import org.scalatest.FunSuite
import org.scalatest._
import Matchers._

class LevelSuite extends FunSuite {

  test("Level loads correctly") {
    val level = Level(List("#####", "#.w.#", "#####"))
    level.width should be(5)
    level.height should be(3)
    level(x = 0, y = 0) should be eq (Wall)
    level(x = 1, y = 1) should be eq (Empty)
    level(x = 3, y = 1) should be eq (Spawn)
  }

  test("Level is updated correctly") {
    implicit val level = Level(List("#####", "#.w.#", "#####"))

    level.setActor(Explorer(1, Position(1, 1)), true)
    level.setActor(Explorer(2, Position(2, 1)))
    level.setActor(Explorer(3, Position(3, 1)))
    level.setActor(Explorer(4, Position(4, 1)))
    level.setActor(Wanderer(5, Position(1, 1)))
    level.setActor(Wanderer(6, Position(1, 1)))

    level.wanderers.length should be(2)
    level.actors.keySet.size should be(6)
    level.explorers.length should be(3)
    level.player.pos should be(Position(1, 1))

    println(level)
  }

  test("Danger Map with spawn is updated correctly") {
    implicit val level = Level(List(
      "##########",
      "#........#",
      "#...w....#",
      "#........#",
      "#........#",
      "#.....w..#",
      "#........#",
      "##########"))
    level.updateDangerHeatMap()
    dump(level)
  }

  test("Danger Map with wanderers is updated correctly") {
    implicit val level = Level(List(
      "##########",
      "#........#",
      "#........#",
      "#........#",
      "#........#",
      "#........#",
      "#........#",
      "##########"))
    level.setActor(Wanderer(1, Position(3,3)))
    level.updateDangerHeatMap()
    dump(level)
  }

  test("Path finding Cache") {

    //Thread.sleep(10000);

    implicit val level = Level(List(
      "###########################",
      "#.........................#",
      "#.#.#.#.#.#.#.#.#.#.#.#.#.#",
      "#.........................#",
      "#.#.#.#.#.#.#.#.#.#.#.#.#.#",
      "#.........................#",
      "#.#.#.#.#.#.#.#.#.#.#.#.#.#",
      "#.........................#",
      "#.#.#.#.#.#.#.#.#.#.#.#.#.#",
      "#.........................#",
      "#.#.#.#.#.#.#.#.#.#.#.#.#.#",
      "#.........................#",
      "#.#.#.#.#.#.#.#.#.#.#.#.#.#",
      "#.........................#",
      "#.#.#.#.#.#.#.#.#.#.#.#.#.#",
      "#.........................#",
      "#.#.#.#.#.#.#.#.#.#.#.#.#.#",
      "#.........................#",
      "###########################"))

    val start = level.pos(1)(1).id
    val end = level.pos(4)(2).id
    level.distanceMap(start)(end) should be (5)

    Thread.sleep(50000);
  }

  test("Path finding") {
    implicit val level = Level(List(
      "##########",
      "#........#",
      "#........#",
      "#........#",
      "#........#",
      "#........#",
      "#........#",
      "##########"))
    level.setActor(Explorer(1, Position(1,1)), true)
    level.setActor(Wanderer(1, Position(4,3)))
    level.updateDangerHeatMap()

    val t1 = System.currentTimeMillis()
    for(i <- 0 to 100000) {
      val y : Int = (1 + Math.random() * 7).asInstanceOf[Int]
      val x : Int = (1 + Math.random() * 7).asInstanceOf[Int]
      level.player.pathTo(Position(x,y), level)
    }
    val t2 = System.currentTimeMillis()
    println((t2-t1))

  }


  test("Danger Map with correct in corners") {
    implicit val level = Level(List(
      "######",
      "#....#",
      "#.##.#",
      "#.#..#",
      "#....#",
      "######"))

    level.setActor(Wanderer(1, Position(3,1)))
    level.setActor(Explorer(2, Position(4,1)), true)

    level.updateDangerHeatMap()
    level.safestPlayerPosition() should be (Position(4,3))

    dump(level)
  }



  test("explorer with count") {
    implicit val level = Level(List(
      "######",
      "#....#",
      "#.##.#",
      "#.#..#",
      "#....#",
      "######"))

    level.setActor(Explorer(1, Position(1,1)), true)
    level.setActor(Wanderer(2, Position(1,3), 1, 400, status = Wanderer.STALKING, 1))
    level.updateDangerHeatMap()

    dump(level)

    var minDanger : Int = Int.MaxValue
    var safestPos : Position = null
    level.explore(Position(1,1), 3, (pos, dst) => {
      val d = level.dangerAt(pos)
      if (d < minDanger) {
        minDanger = d
        safestPos = pos
      }
    })

    println("safe position is at " + safestPos)
}

  private def dump(level: Level) = {
    println(level.dangerMap.map(_.mkString(",")) mkString ("\n"))
  }
}