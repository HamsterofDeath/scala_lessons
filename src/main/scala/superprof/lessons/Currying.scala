package superprof.lessons

import scala.math.Ordering.BooleanOrdering

object Currying {

  def f(a:Int,b:Int):Int = {
           a+b
  }

  def f(a:Int):Int = {
    f(a,1)
  }


  def main(args: Array[String]): Unit = {
    val function : (Int,Int) => Int = (a,b) => a+b
    val function2  = (a:Int,b:Int) => a+b
    def mult1(a:Int,b:Int) = a*b
    def mult2(a:Int)(b:Int) = a*b

    def addAllKomma(a: Int,b: Int,c: Int,d: Int,e: Int,f: Int) = "hallo"
    def addAll(a: Int)(b: Int)(c: Int)(d: Int)(e: Int)(f: Int): String =
      s"hallo$a$b$c$d$e$f"

    val geht = addAllKomma(_,_,3,_,_,_)

    addAll(_:Int)(_:Int)(3)(_:Int)(_:Int)(_:Int)



    val step1: Int => Int => Int => Int => Int => String = addAll(1)
    val step2: Int => Int => Int => Int => String        = step1(2)
    val step3: Int => Int => Int => String = step2(3)
    val step4: Int => Int => String                      = step3(4)
    val step5: Int => String = step4(5)
    val step6: String                                    = step5(6)

    val delegate: (Int, Int, Int, Int, Int) => String = addAllKomma(1,_,_,_,_,_)
    val delegate1                                     = delegate(2,_,_,_,_)

    def spiceItUp(f:(Int,Int) => Int): Int => Int => Int = {
      def curry(a:Int) = (b:Int) => f(a,b)
      curry
    }

    def spiceItUp2(f:(Int,Int) => Int): Int => Int => Int = { //anonym
      (a:Int) => {
        (b:Int) => {
          f(a,b)
        }
      }
    }
    def unCurry (f:Int => Int => Int): (Int,Int) => Int =
      def unCurrying (x:Int, y:Int): Int = f(x)(y)
      unCurrying(_,_)

    def unCurry2 (f:Int => Int => Int): (Int,Int) => Int =
       (x:Int, y:Int) => f(x)(y)


    val testF     : (Int, Int) => Int = (a:Int, b:Int) => a * b
    val curryified: Int => Int => Int = spiceItUp(testF)

    testF(1,2)
    curryified(1)(2)

    def isEven(n:Int, debug:Boolean) = {
      val ret = n % 2 == 0
      if(debug) println(s"$n ist gerade=$ret")
      ret
    }

    val debuggingCheck = isEven(_,true)
    debuggingCheck(1)
    debuggingCheck(1)
    debuggingCheck(1)
    debuggingCheck(1)
    debuggingCheck(1)
    debuggingCheck(1)

    val f1: Int => Int        = f(_,1)
    val fc: (Int, Int) => Int = f(_,_)

    def modifyList(l: List[Int], f: Int => Int): List[Int] = l match
      case x :: xs => f(x) :: modifyList(xs, f)
      case List() => List()

    def modifyList2(l: List[Int], f: Int => Int): List[Int] = l.map(f)


    val l : List[Int] = List(1, 2, 3, 4, 5)
    val l2: List[Int] = modifyList(l,_+5)

  }
}
