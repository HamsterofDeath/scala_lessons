package superprof.lessons

object BinaryToHex extends App {

  val binary = "1101101110111"

  val hexaDecimal = Integer.parseInt(binary,2).toHexString

  println(hexaDecimal)
}

object Main extends App {
  def fibonacci(n: Int): Int = {
    // initialize a sequence with the first two Fibonacci numbers
    var fibSeq = Seq(1, 1)

    // iterate over the sequence, starting at the third number
    for (i <- 2 until n) {
      // calculate the next Fibonacci number in the sequence
      val next = fibSeq(i - 1) + fibSeq(i - 2)
      // append the next number to the sequence
      fibSeq :+= next
    }

    // return the nth Fibonacci number
    fibSeq(n - 1)
  }

  // calculate the 1001st Fibonacci number
  val fibonacci1001 = fibonacci(6)

  // print the result
  println(fibonacci1001)

}