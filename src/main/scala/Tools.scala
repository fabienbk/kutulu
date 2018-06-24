object Tools {

  def markLoS(start: Position, end : Position, level: Level, value: Int) = {
    val map = level.dangerMap
    var curr = start;

    while(curr != end) {
      map(curr.y)(curr.x) = value
      if (start.x < end.x && start.y == end.y) curr = curr.right
      else if (start.x > end.x && start.y == end.y) curr = curr.left
      else if (start.y > end.y && start.x == end.x) curr = curr.above
      else if (start.y < end.y && start.x == end.x) curr = curr.below
    }
    map(curr.y)(curr.x) = value
  }


  def blurDanger(wpos: Position, level: Level, value: Int): Unit = {
    var x = wpos.x
    var y = wpos.y
    val map = level.dangerMap

    val v1 = value/4
    val v2 = v1/4
    val v3 = v2/4

    if (y-3>=0) map(y-3)(x) += v3
    if (y+3<level.height) map(y+3)(x) += v3
    if (x-3>=0) map(y)(x-3) += v3
    if (x+3<level.width) map(y)(x+3) += v3

    if (y-2>=0) map(y-2)(x) += v2

    map(y-1)(x-1) += v2
    map(y-1)(x) += v1
    map(y-1)(x+1) += v2

    if (x-2>=0) map(y)(x-2) += v2
    map(y)(x-1) += v1
    map(y)(x) += value
    map(y)(x+1) += v1
    if (x+2<level.width) map(y)(x+2) += v2

    map(y+1)(x-1) += v2
    map(y+1)(x) += v1
    map(y+1)(x+1) += v2

    if (y+2<level.height) map(y+2)(x) += v2
  }

}
