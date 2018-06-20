object Tools {

  def blur(grid: Array[Array[Int]],
           buffer: Array[Array[Int]]) = {

    for(y <- 1 until grid.length - 1)
      for(x <- 1 until grid(y).length - 1) {
        val i = grid(y - 1)(x - 1) +
          grid(y - 1)(x) +
          grid(y - 1)(x + 1) +
          grid(y)(x - 1) +
          grid(y)(x) +
          grid(y)(x + 1) +
          grid(y + 1)(x - 1) +
          grid(y + 1)(x) +
          grid(y + 1)(x + 1)
        buffer(y)(x) = i / 9
      }
  }

}
