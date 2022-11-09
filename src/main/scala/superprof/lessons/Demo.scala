package superprof.lessons

def funktion = 5
def funktion2: Int = 5

def funktion3(a:Int) = a
def funktion3(a:Int,b:Int) = {
  val temp = a+1

  temp + b
  7
}
def funktion4(a:Int,b:Int) =
  val temp = a+1
  temp + b
  7

def isVowel(c: Char) = {
  c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u'
}

def patternMatchExample(n:Int,m:Int) = {
  n match {
    case 1 => println("hallo")
    case 2 => println("ciao")
    case _ if n > m => println("n>m")
    case _  => println("unknown")
  }


}

def patternMatchExample(l:List[Int]) = {
  println(s"Checking $l")
  l match {
    case List(1,2,_,_) => println("ich kenn dich")
    case List(1,2,_*) => println("ich kenn dich auch")
    case 0 :: 1 :: 2 :: Nil => println("ich kenn dich auch v2")
    case hallo :: 1 :: 2 :: Nil => println(s"ich kenn dich auch v2: $hallo")
    case _  => println("nein :(")
  }
  val alter = 5
  val s = s"ich bin $alter jahre alt aber nicht mehr ${alter-1}"

  val p = Person("ich",5)
  p match {
    case Person(_,checkMich) if checkMich < 18 => println(checkMich)
  }

}


case class Person(name:String,alter:Int)

@main def demo2 = {
  patternMatchExample(10, 100)
  patternMatchExample(List(1))
  patternMatchExample(List(1,2))
  patternMatchExample(List(0,1,2))
  patternMatchExample(List(1,2,-1,-1))
  patternMatchExample(List(999,2,-1,-1))
}


extension (s:String) {
  def countVowels = {
    def isVowel2(c:Char) = {
      c match {
        case 'a' | 'e' | 'i' |'o'|'u' => true
        case _ => false
      }
    }

    def isVowel3(c:Char) = {
      val set = Set('a','e','i','u','o')

      set(c)
    }

    s.count(isVowel)
  }

  def filterVowels = {
    s.filter(isVowel)
  }
}


@main def hallo = {
  println("hallo".countVowels)
  println("hallo".filterVowels)
  println("hallo".map(c => c.toUpper))
  val numbers = List(1,2,3,4)
  val numbersAnd0 = 0 :: numbers
  val numbersSet = Set(1,2,3,4)
  numbers ++ numbersSet
  val list = 1::2::Nil
  val neueListe = 7 :: numbers
  val neueListe2 = numbers.::(7)
  val neueListe2b = numbers.appended(7)
  val neueListe2c = numbers :+ 7
  val neueListe3 = List(45,46) ::: numbers
  println("hallo".map(c => c.toUpper))

  val liste: List[(Int, Int, String, Int, Int)] = List[(Int, Int, String, Int, Int)]((5,6,"x",8,9),(1,6,"x",8,9),(5,5,"y",8,9))
  println()

  val tuple = magie(5.0)
  println(tuple)

  List[Int]
}

def magie[HALLO](ding:HALLO):(HALLO,HALLO) = {
  (ding, ding)
}

@main def demoX = {
  val functionA = (x: Int) => x + 1
  val functionAComplete = new Function[Int,Int] {
    override def apply(v1: Int): Int = v1+1
  }

  val p1 = Person("test",115)

  def functionB(x: Int) = x + 1

  functionA(10)

  List(1, 2, 3, 4).map(functionAComplete)

}
