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

  test("Populated") {
    val level = Level(List("#####", "#.w.#", "#####"))

    level.addActor(Explorer(Position(1,1)), true)
    level.addActor(Explorer(Position(2,1)))
    level.addActor(Explorer(Position(3,1)))
    level.addActor(Explorer(Position(4,1)))
    level.addActor(Wanderer(Position(1,1)))
    level.addActor(Wanderer(Position(1,1)))

    level.wanderers.length should be (2)
    level.actors.length should be (6)
    level.explorers.length should be (3)
    level.allExplorers.length should be (4)

    level.player.pos should be (Position(1,1))
  }


}