package superprof.lessons

object KVA extends App {
  type Snake = List[Int]
  val snake = List(3, 2, 2, 3, 2, 3, 2, 2, 3, 3, 2, 2, 2, 3, 3, 3, 3)
  //1
  def checkValidity(s: Snake): Boolean =
    s.sum - (s.length + 1) == 27

  println(checkValidity(snake)) //> true

  //2
  type Position = (Int, Int, Int)
  type Direction = (Int, Int, Int)

  val P1: Position = (1, 4, 3)

  def inCube(dim: Int)(p: Position): Boolean = //dimnesion vom Würfel

    def inRange(n: Int) = 1 <= n && n <=
                                    dim // Minimaler wert ist 1 (Kooridinaten (1,1,1) max Wert
    // von der Dimension

    val (x, y, z) = p
    inRange(x) && inRange(y) && inRange(z)

  println(inCube(3)(P1))

  val inCube3 = inCube(3) _
  val inCube4 = inCube(4) _

  println(inCube3(P1))

  //c)
  type Section = List[Solution]
  type Solution = List[Position]

  //d)
  def section(star: Position, dir: Direction, len: Int): Section =
    val (u, v, w) = dir
    val pieces = Iterator.iterate(star) (p => p match
      case (x,y,z)=> (x+u, y+v, z+w))
    List(pieces.take(len).toList.tail.reverse)
  //e)
  def newDirs(dir: Direction): List[Direction] = dir match
    case (x, y, z) => List((y, z, x), (-y, -z, -x), (z, x, y), (-z, -x, -y))
}
