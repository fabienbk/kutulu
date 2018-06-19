object Tools {

  def blur(g: Array[Array[Int]], iterations: Int) = {
    for(y <- 1 until g.length - 1)
      for(x <- 1 until g(y).length - 1)
        g(y)(x) =
          (g(y-1)(x-1) +
          g(y-1)(x) +
          g(y-1)(x+1) +

          g(y)(x-1) +
          g(y)(x) +
          g(y)(x+1) +

          g(y+1)(x-1) +
          g(y+1)(x) +
          g(y+1)(x+1)) /9
  }

}
