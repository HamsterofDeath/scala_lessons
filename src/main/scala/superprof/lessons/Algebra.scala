package superprof.lessons

object Algebra extends App {

  type Beruf = "Arzt" | "Doktor" | "Rennfahrer"

  case class Person(name: String, beruf: Beruf)

  Person("hans", "Arzt")

  sealed trait Beruf2 {
    def bezeichnung: String
  }

  case class Arzt(sparte: String) extends Beruf2 {
    override def bezeichnung: String = s"Arzt vom untertyp $sparte"
  }
  case object Doktor extends Beruf2 {
    override def bezeichnung: String = "Doktor"
  }

  def doStuff(b: Beruf2) = {
    println(b.bezeichnung)
  }

  doStuff(Arzt("zahn"))
  doStuff(Doktor)

  enum Farbe:
    case Rot
    case Blau
    case Green

  enum Wochentag:
    case Mo
    case Di
    case Mi
    case Do
    case Fr
    case Sa
    case So

  private val values: Array[Wochentag] = Wochentag.values
  Wochentag.fromOrdinal(5)
  private val mo: Wochentag = Wochentag.valueOf("Mo")
  println(mo)

  println(Wochentag.Mo.toString)



  class Demo(val x: Int) {
    def y = x + 1
  }

  val d = new Demo(5)

  trait TWochentag(next: TWochentag) {
    def nextDay = next
  }

  case object TMo extends TWochentag(TDi)
  case object TDi extends TWochentag(TMi)
  case object TMi extends TWochentag(TDo)
  case object TDo extends TWochentag(TFr)
  case object TFr extends TWochentag(TSa)
  case object TSa extends TWochentag(TSo)
  case object TSo extends TWochentag(TMo)

  TMi.nextDay.nextDay

  enum Dreieck:
    case Entartet, Gleichseitig, Gleichschenklig, Normal

  def getType(a:Int,b:Int,c:Int) = {
    val List(a1,b1,c1) = List(a, b, c).sorted
    val a2 = a min b min c
    val c2 = a max b max c



  }

  class Human(name:String, vater:Option[Human], mutter: Option[Human]) {
    def useIt = s"$name $vater $mutter"
  }

  val f = Human("meine mutter", None, None)
  val m = Human("mein vater", None, None)
  val ich = Human("Prof", Some(m), Some(f))
  val meinKind = Human("sohn",Some(ich),None)



  println(ich)


}
