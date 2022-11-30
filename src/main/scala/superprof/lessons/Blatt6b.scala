package superprof.lessons

object Blatt6b extends App {
  val numbers = List(1, 2, 3, 4)
  val chars   = List('a', 'b', 'c', 'd', 'e')
  val colors  = List("schwarz", "weiss", "rot")

  val demo = List(1, 2, 3, 4, 5).flatMap(e => List(e, e, e))
  println(demo)

  val triples = {
    numbers.flatMap { e =>
      chars.flatMap { c =>
        colors.map { col =>
          s"$e$c - $col"
        }
      }
    }
  }

  val besser = {
    for {
      num <- numbers
      c <- chars if c > 1
      col <- colors
    } yield {
      s"$num$c - $col"
    }
  }
  println(triples)
  println(besser)

  val nested = List(List(1), List(1)).flatten

  def generator(l: List[List[Any]]): List[List[Any]] = {
    l match {
      case Nil => Nil
      case h :: _ => h.flatMap(i => generator(l.tail).map(i :: _))
    }
  }

  println(generator(List(List(1), List(1))))

  type Kassenzetteleintrag = (Int, String, Double) //Menge, Name, Preis
  type Kassenzettel = List[Kassenzetteleintrag]

  val dummy = List((1,"ei vom huhn",40.0),(1,"ei vom ente",50.0))

  def replaceSpace(k: Kassenzettel): Kassenzettel = {
    k.map {
      (m, n, p) => (m,n.map(e => if(e.isWhitespace) '_' else e), p)
    }
  }


  println(replaceSpace(dummy))

  def teurerAls(k: Kassenzettel, p: Double): List[Kassenzetteleintrag] = {
    k.filter { (m,_,pe) => m*pe > p}
  }

  println(teurerAls(dummy, 39))
  println(teurerAls(dummy, 41))

  def preis(k: Kassenzettel): Double = {
    k.map((m,_,p) => m*p).reduce(_ + _)
  }

  def preisF(k: Kassenzettel): Double = {
    k.map((m,_,p) => m*p).fold(0.0)(_ + _)
  }
  def preisFLR(k: Kassenzettel): String = {
    k.map((m,_,p) => m*p).foldLeft(0.0)(_ + _)
    k.map((m,_,p) => m*p).foldRight(0.0)(_ + _)

    k.map((m,_,p) => m*p).foldLeft("hallo")((acc,e) => {
      println(s"Called with $acc + $e")
      s"$acc und dann noch $e"
    })
    k.map((m,_,p) => m*p).foldRight("hallo")((e,acc) => {
      println(s"Called with $acc + $e")
      s"$acc und dann noch $e"
    })
  }

  println(preisFLR(dummy))

  def maxSort(num:List[Int]): Option[Int] = num.reduceOption(_ max _)
  def maxSortR(num:List[Int]) = num.reduce(_ max _)

  def maxOfList[T](kg: (T, T) => Boolean)(list: List[T]): T = {
    list.reduce((l,r) => if(kg(l,r)) r else l)
  }

  println(maxOfList[Int](_ <= _)(List(1,3,5,7,6,4,99,43,4,3,2,1,5)))

  def streiche[T](gleich: (T, T) => Boolean)(x: T, list: List[T]): List[T] = {
    var alreadyKicked = false
    list.filter { e =>
      if(!alreadyKicked && gleich(e,x)) {
        alreadyKicked = true
        false
      } else {
        true
      }
    }
  }

  println (streiche[Int](_ == _)(5, List(1,3,5,7,6,4,99,43,4,3,2,1,5)))

  def streicheFunktional[T](gleich: (T, T) => Boolean)(x: T, list: List[T]): List[T] = {
    list.foldLeft((List.empty[T], false))((tup,e) => {
      val(acc,hasKickedElement) = tup
      val addThisElement = !gleich(x,e) || hasKickedElement
      val newList = if(addThisElement) {
        (acc :+ e)
      } else {
        acc
      }
      (newList, hasKickedElement || gleich(x,e))
    })._1
  }
  println("test")
  println (streicheFunktional[Int](_ == _)(5, List(1,3,5,7,6,4,99,43,4,3,2,1,5)))

  def maxSort2[T](kg: (T, T) => Boolean, gleich: (T, T) => Boolean)(
    list: List[T]): List[T] = {
    if(list.isEmpty)
      Nil
    else {
      val max = maxOfList(kg)(list)
      val allesAusserMax = streicheFunktional(gleich)(max, list)
      max :: (maxSort2(kg, gleich)(allesAusserMax))
    }
  }

  println(streicheFunktional[Int](_ ==_)(99, List(1,3,5,7,6,4,99,43,4,3,2,1,5)))
  println(maxSort2[Int](_ <= _,_ ==_)(List(1,3,5,7,6,4,99,43,4,3,2,1,5)))

}
