import org.scalatest.FunSuite
import org.scalatest._
import Matchers._

class LevelSuite extends FunSuite {

  test("Level loads correctly") {
    val level = Level(List("#####", "#.w.#", "#####"))
    level.width should be (5)
    level.height should be (3)
    level(x=0, y=0) should be eq(Wall)
    level(x=1, y=1) should be eq(Empty)
    level(x=3, y=1) should be eq(Spawn)
  }

  test("Level is updated correctly") {
    val level = Level(List("#####", "#.w.#", "#####"))

    level.setActor(Explorer(1,Position(1,1)), true)
    level.setActor(Explorer(2,Position(2,1)))
    level.setActor(Explorer(3,Position(3,1)))
    level.setActor(Explorer(4,Position(4,1)))
    level.setActor(Wanderer(5,Position(1,1)))
    level.setActor(Wanderer(6,Position(1,1)))

    level.wanderers.length should be (2)
    level.actors.keySet.size should be (6)
    level.explorers.length should be (3)
    level.allExplorers.length should be (4)

    level.player.pos should be (Position(1,1))

    println(level)
  }

  test("Danger Map is updated correctly") {

    val level = Level(List(
      "#######",
      "#.....#",
      "#######"))


  }