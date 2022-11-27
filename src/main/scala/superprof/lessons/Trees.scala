package superprof.lessons

import superprof.lessons.Trees.Baum.Knoten

object Trees extends App {
  enum Baum[T]:
    case Knoten(wert: T, li: Baum[T], re: Baum[T])
    // 3-stellig, rekursiv
    case Leer()
    // 0-stellig, nicht rekursiv

  val tree: Baum[String] = Baum.Knoten("5", Baum.Knoten("4", Baum.Leer(), Baum.Leer()), Baum.Leer())

  def enthaelt[T](t:T,baum:Baum[T]):Boolean = {
    println(s"Wir checken ${baum}")
    baum match
      case Baum.Knoten(wert, _, _) if wert == t => true
      case Baum.Knoten(_, li, re) => enthaelt(t, li) || enthaelt(t,re)
      case Baum.Leer() => false
  }
  println(enthaelt("6", tree))


  def foreach[T](baum: Baum[T],f:T => Unit):Unit = {
    baum match
      case Baum.Knoten(wert, li, re) => {
        foreach(li,f)
        f(wert)
        foreach(re,f)
      }
      case Baum.Leer() =>
  }
  def ein[T](isFirstArgumentSmaller:(T,T) => Boolean)(t:T,baum:Baum[T]):Baum[T] = {
    baum match
      case k@Baum.Knoten(wert, _, re) if isFirstArgumentSmaller(wert,t) =>
        k.copy(re = ein[T](isFirstArgumentSmaller)(t, re))
      case k@Baum.Knoten(wert, li, _) if isFirstArgumentSmaller(t, wert) =>
        k.copy(li = ein[T](isFirstArgumentSmaller)(t, li))
      case Baum.Leer() => Baum.Knoten(t,Baum.Leer(),Baum.Leer())
      case _ => baum
  }

  val leer                 = Baum.Leer[Int]()
  val baumMit10: Baum[Int] = ein[Int](_ < _)(10, leer)
  println(baumMit10)

  private val mit10Und11: Baum[Int] = ein[Int](_ < _)(11, baumMit10)
  println(mit10Und11)

  val knotenMit10: Baum.Knoten[Int] = Baum.Knoten(10, Baum.Leer(), Baum.Leer())

  val knotenMit11: Baum.Knoten[Int] = Baum.Knoten(11, Baum.Leer(), Baum.Leer())
  val mit10und11b =  knotenMit10.copy(re = knotenMit11)

  val knotenMit12: Baum.Knoten[Int] = Baum.Knoten(12, Baum.Leer(), Baum.Leer())
  val mit10und11und12 = knotenMit10.copy(re = knotenMit11.copy(re = knotenMit12))

  println(mit10und11b)
  println(mit10und11und12)

  val insert = List("Alice", "Bob","Eve", "Trent", "Mallory","Apple")

  var baum = Baum.Leer[String]()
  baum = ein[String](_<_)("Alice", baum)
  baum = ein[String](_<_)("Bob", baum)
  baum = ein[String](_<_)("Eve", baum)
  baum = ein[String](_<_)("Trent", baum)
  baum = ein[String](_<_)("Mallory", baum)
  baum = ein[String](_<_)("Apple", baum)

  insert.foreach{name =>
    baum = ein[String](_ < _)(name, baum)
  }
  foreach(baum,println)
}
