package superprof.lessons

object EnumerationStuff {
  val ROT = 1
  val BLAU = 2
  val VIOLETT = 3

  val ampelZustand = 4

  enum Farbe(val istHell:Boolean, val helligkeit:Int):

    def istHellGenug = helligkeit > 50

    case Rot extends Farbe(true,100)
    case Blau extends Farbe(true,50)
    case Violet extends Farbe(false,-5)

  val x:Farbe = Farbe.Rot

  class Person(val name:String) {
    def hatKinder = false
  }

  class Mutter(name:String, anzahlKinder:Int) extends Person(name:String) {
    override def hatKinder: Boolean = anzahlKinder > 0
  }

  val p:Person = new Mutter("hansine",5)
  p.hatKinder

  def isHell(f:Farbe) = f match
    case Farbe.Rot => false
    case Farbe.Blau => true
    case Farbe.Violet => math.random()>0.5

  isHell(Farbe.Rot)

  Farbe.Rot.istHellGenug
}
