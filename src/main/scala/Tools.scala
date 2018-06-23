object Tools {

  def blurDanger(wpos: Position, level: Level): Unit = {
    var x = wpos.x
    var y = wpos.y
    val map = level.dangerMap

    if (y-2>=0) map(y-2)(x) += 25

    map(y-1)(x-1) += 25
    map(y-1)(x) += 100
    map(y-1)(x+1) += 25

    if (x-2>=0) map(y)(x-2) += 25
    map(y)(x-1) += 100
    map(y)(x) += 400
    map(y)(x+1) += 100
    if (x+2<level.width) map(y)(x+2) += 25

    map(y+1)(x-1) += 25
    map(y+1)(x) += 100
    map(y+1)(x+1) += 25

    if (y+2<level.height) map(y+2)(x) += 25
  }

}
