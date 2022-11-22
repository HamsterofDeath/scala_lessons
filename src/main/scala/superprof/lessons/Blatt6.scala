package superprof.lessons

object Blatt6 {

  def map[A, B](in: List[A])(f: A => B): List[B] = {
    println(in)
    in match
      case head :: tail =>
        val mappedHead = f(head)
        val mappedTail = map(tail)(f)
        mappedHead :: mappedTail
      case Nil => Nil
  }

  def main(args: Array[String]): Unit = {
    println(map(List(1, 2, 3, 4))(_ * 10))
    println(map(List(1, 2, 3, 4))(_.toBinaryString))
    println(map(List(1, 2, 3, 4))(e => e * 10))

    type Eintrag = (Int, String, Double)
    type KassenZettel = List[Eintrag]

    val zettels = List(
      (1,"apfel",10.0),
      (1,"banane",20.0),
    )

    def doIt(tup:Eintrag): (Int, String, Double) = tup match {
      case org@(stueck,_,_) if stueck<10 => org.copy(_1 = stueck+1)
      case org => org
    }
    println(map(zettels)((stueck,name,preis) => {
      (stueck + (if(stueck<10) 1 else 0),name,preis)
    } ))

    println(map(zettels)(doIt))

    val person = Person(alter = 0, name = "???")
    val person1   = person.copy(alter = 5)
  }

}

object Kartenspiel {
  enum Farbe:
    case Kreuz, Pik, Herz, Karo
  enum Wert:
    case Sieben, Acht, Neun, Bube, Dame, Koenig, Zehn, As

  type Karte = (Farbe, Wert)

  def zaehlwert(n : Wert): Int = {
    n match
      case Wert.Sieben => 0
      case Wert.Acht => 0
      case Wert.Neun => 0
      case Wert.Bube => 2
      case Wert.Dame => 3
      case Wert.Koenig => 4
      case Wert.Zehn => 10
      case Wert.As => 11
  }

  def kartenWert(k:Karte): Int = zaehlwert(k._2)

  def meinStich(k1:Karte,k2:Karte) = {
    val ((meineFarbe,meinWert),(deineFarbe,deinWert)) = (k1,k2)
    meineFarbe!=deineFarbe || zaehlwert(meinWert) > zaehlwert(deinWert)
  }



  def main(args: Array[String]): Unit = {
    println(meinStich((Farbe.Herz, Wert.Dame),(Farbe.Herz, Wert.Koenig)))
    println(meinStich((Farbe.Karo, Wert.Dame),(Farbe.Herz, Wert.Koenig)))
    println(meinStich((Farbe.Herz, Wert.Koenig),(Farbe.Herz, Wert.Dame)))
  }
  type Stich = (Karte, Karte)

  def stichSumme(x:List[Stich]): Int = {
    def wertDesStiches(s:Stich) = {
      val (k1, k2) = s
      if meinStich(k1,k2) then kartenWert(k1) else 0
    }
    val werte = Blatt6.map(x)(wertDesStiches)
    werte.reduce(_ + _)
  }
  val stich = ((Farbe.Herz, Wert.As),(Farbe.Herz, Wert.Koenig))
  println(stichSumme(List(stich, stich, stich)))


}
