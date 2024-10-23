package lectures.part1.as

import scala.util.Try

object DarkSugars extends App{

  // syntax sugar #1 :  methods with single param
  def singleArgMethod(arg: Int): String = s"$arg little ducks ...."

  val description = singleArgMethod{
    //write some complex code
    42
  }

  val aTryInstance = Try { // java's try {...}
    throw new RuntimeException()
  }

  List(1,2,3).map { x =>
    x + 1
  }

  // syntax sugar #2 single abstract method (instead of writing full on anonymous fxn convert LAMBDA -> anonymous fx)
  trait Action{
    def act(x: Int): Int
  }

  val aInstance: Action = new Action{
    override def act(x: Int): Int = x + 1
  }

  val aFunkyInstance: Action = (x: Int) => x + 1 //magic to convert this LAMBDA -> anonymous fxn (NOTE **: only for single abstract method)

  //eg: Runnables(like Java)
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("hello, Scala")
  })

  val aSweeterThread = new Thread(() => println("sweet, scala"))

  abstract class AnAbstractType{
    def implemented: Int = 23
    def f(a: Int): Unit
  }

  val anAbstractInstance: AnAbstractType = (a: Int) => println("sweet")

  // syntax sugar #3 :  the  :: & #:: METHODS are special

  val prependedList = 2 :: List(3, 4) //2.::(List(3,4)) wrong instead -> List(3,4).::(2) ?!

  // scala spec: last char decide associativity of method
  1 :: 2:: 3 :: List(4, 5)
  List(4, 5).::(3).::(2).::(1) // equivalent

  class MyStream[T]{
    def -->:(value: T) : MyStream[T] = this //actual implementation
  }

  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  // syntax sugar #4 : multi-word method naming (RARELY Used)

  class TeenGirl(name: String){
    def `and then said`(gossip: String) = println(s"$name said $gossip")
  }

  val lilly = new TeenGirl("lilly")
  lilly `and then said` "Scala is so sweet!"

  // syntax sugar #5: infix types
  class Composite[A, B]
  val composite: Int Composite String = ???

  class -->[A, B]
  val towards: Int --> String = ???

  //syntax sugar #6: update() us very special, much like apply()
  val anArray = Array(1, 2, 3)
  anArray(2) = 7 //rewritten to anArray.update(2, 7)
  //used in mutable collection
  //remember apply() AND update()

  // syntax sugar #7: setter for mutable containers
  class Mutable{
    private var internalMember: Int = 0// private for OO encapsulation
    def member = internalMember// "getter"
    def member_=(value: Int): Unit =
      internalMember = value
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 42 //rewritten as aMutableContainer.member_=(42)



}
