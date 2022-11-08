package superprof.lessons

import org.scalatest.flatspec.AnyFlatSpec

class ObjectTest extends AnyFlatSpec {
  "The object" should "add 1 + 1" in {
    val sum = AnObject.sum(1,1)
    assertResult(2)(sum)
  }
}
