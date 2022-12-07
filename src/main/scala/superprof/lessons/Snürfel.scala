package superprof.lessons

object Snürfel extends App {
  val sectionSizes      = List(3, 2, 2, 3, 2, 3, 2, 2, 3, 3, 2, 2, 2, 3, 3, 3, 3)
  val better: List[Int] = sectionSizes.map(_ - 1)
  case class XYZ(x: Int, y: Int, z: Int) {
    def +(other: XYZ) = XYZ(x + other.x, y + other.y, z + other.z)
  }

  def insideCube(xyz: XYZ) = {
    xyz.x >= 0 && xyz.x < 3 &&
    xyz.y >= 0 && xyz.y < 3 &&
    xyz.z >= 0 && xyz.z < 3
  }

  val alongX = List(XYZ(-1, 0, 0), XYZ(1, 0, 0))
  val alongY = List(XYZ(0, -1, 0), XYZ(0, 1, 0))
  val alongZ = List(XYZ(0, 0, -1), XYZ(0, 0, 1))

  def recur(sectionsLeft: List[Int], blocked: List[XYZ]): Unit = {
    if (sectionsLeft.isEmpty) {
      println(s"Solution: ${blocked.reverse}")
    } else {
      val turnHere    = blocked.head
      val before      = blocked.tail.head
      val nextOptions = {
        if (turnHere.x != before.x) {
          // nach rechts oder links
          alongY ++ alongZ
        } else if (turnHere.y != before.y) {
          alongX ++ alongZ
        } else if (turnHere.z != before.z) {
          alongX ++ alongY
        } else {
          throw new RuntimeException(":(")
        }
      }

      nextOptions.foreach { nextOption =>
        def nextSectionCoordinates = Iterator.iterate(turnHere + nextOption)(_ + nextOption)
                                             .take(sectionsLeft.head)

        val validChoice = nextSectionCoordinates.forall { xyz =>
          !blocked.contains(xyz) && insideCube(xyz)
        }

        if (validChoice) {
          recur(sectionsLeft.tail, nextSectionCoordinates.toList.reverse ++ blocked)
        }

      }
    }
  }

  recur(better.tail, List(XYZ(2, 0, 0), XYZ(1, 0, 0), XYZ(0, 0, 0)))

}
