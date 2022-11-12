package superprof.lessons

import java.math.BigInteger
import scala.math.Ordering

object Blatt51 extends App{
  def summe(x:Int,y:Int) = x + y
  def summeXY(x:Int)(y:Int) = x + y

  summe(1,2)
  private val intToInt: Int => Int = summeXY(1)
  intToInt(2)

  val mit5alsX = summe(5,_)

  val explicit = (y:Int ) => summe(5,y)

  def measureTime(name:String)(stuff : => Unit) = {
    println(s"Starte operation $name")
    stuff
    println(s"Beende operation $name")
  }

  val stepInBetween = measureTime("was ich mache")

  stepInBetween(println("hey"))

  def function(name:String, desc:Option[String]):Unit = {
  }

  def function(name:String ):Unit = function(name, None)

  def f = function(_, None)

  function("hallo", None)
  function("hallo2", None)
  function("hallo3", None)

  val add2Number: (Int, Int) => Int = (x:Int, y:Int) => x + y
  def add2Numbers(x:Int,y:Int) = x+y

  def functionThatTakesAFunction(a:List[Int],f:(Int,Int) => Int) = {
    a.reduce(f)
  }

  println(functionThatTakesAFunction(List(1,2,3,4,5), (a,b) => a*b))

  def xyz = println("hey")

  measureTime("test") {
    println("ich habe keinen namen")
  }
  measureTime("test")(xyz)

  val func = (x:Int) => x+1



  f("hallo")
  f("hallo2")
  f("hallo3")

  def buildList[TYPEPARAMETER <: Number](a:TYPEPARAMETER,b:TYPEPARAMETER):List[TYPEPARAMETER] = List(a,b)

  case class X(a:Int)
  private val value : List[BigInteger]    = buildList(BigInteger.valueOf(1L), BigInteger.valueOf(1L))
  //private val value1: List[String] = buildList("t1","t2")

  type Vorname = String
  type Nachname = String

  case class Person(vorname: Vorname, nachname: Nachname)
  val vn:Vorname = "peter"
  val nn:Nachname = "müller"

  val vn2:Vorname = nn

  type complex = (Int,String, BigInteger,String)

  def takesComplex(c:complex) = {

  }

  case class CCEintrag(menge:Int, name:String, preis:Double) {
    def gesamtpreis = menge * preis
  }
  case class CCKassenZettel(eintraege:List[CCEintrag]) {
    def gesamtpreis = {
      eintraege.map(_.gesamtpreis).sum
    }
  }

  type Eintrag = (Int, String, Double)
  type KassenZettel = List[Eintrag]

  def kgAnzahl(left:Eintrag, right:Eintrag) = {
    left._1 <= right._1
  }

  def kgAnzahl(left:CCEintrag,right:CCEintrag) = {
    left.menge <= right.menge
  }

  case class Etwas(str:String)

  val e = new Etwas("hey")
  val e2 = new Etwas("hey")
  println(e.equals(e2))
  println(e == e2)

  def gleich(a:CCEintrag,b:CCEintrag) = {
    a.menge == b.menge && a.preis == b.preis && a.name == b.name
  }

  val examples : List[CCEintrag] = Nil

  val z1 = List(
    CCEintrag(50, "Benzin", 1.37),
    CCEintrag(25, "Benzin", 1.37),
    CCEintrag(75, "Benzin", 1.37),
    CCEintrag(2, "Scheibenwischer", 5.00),
    CCEintrag(1, "Autowaesche", 10.00),
    CCEintrag(1, "Kaugummi", 0.99))

  println(z1.sortBy(cc => (cc.name,cc.menge,cc.preis)))

  List.empty[CCKassenZettel].sortBy(_.gesamtpreis)

}

object Blatt51b extends App {

  def maxOf[T](listOfTs: List[T], functionThatJustKnows:(T,T) => T) = {
    listOfTs.reduce(functionThatJustKnows)
  }


  def streiche[T](y: T, l: List[T]) = {
    l.indexOf(y) match {
      case -1 => l
      case n => l.take(n) ++ l.drop(n + 1)
    }
  }

  def maxSort[T](l: List[T],getMax:(T,T) => T): List[T] = {
    println(s"called with $l")
    if (l.isEmpty)
      l
    else {
      val sortedRecursively = {
        val maxOfL     = maxOf(l, getMax)
        val withoutMax = streiche(maxOfL, l)
        maxSort(withoutMax, getMax)
      }
      val highestElement                 = {
        println(s"attaching ${maxOf(l, getMax)}")
        maxOf(l, getMax)
      }
      sortedRecursively :+ highestElement
    }
  }

  println(maxSort(List("a", "ba", "bab", "z"),
    (a: String, b: String) => if (a.size > b.size) a else b)
  )

  given x:Int = 42
  def magic(using  x:Int) = println(x)


  println(List((0,2),(1,0),(0,1)).sorted)


}
