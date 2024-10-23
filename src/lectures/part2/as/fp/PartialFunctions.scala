package lectures.part2.as.fp

object PartialFunctions extends App {
  //"PARTIAL FXN" = only accepts certain values (based on  pattern matching)
  val aFunction = (x: Int) => x + 1 // Function1[Int, Int] === Int => Int

  //total fxn
  val aFussyFunction = (x:Int) =>
    if(x == 1) 42
    else if (x == 2) 56
    else if (x == 5) 99
    else throw new FunctionNotApplicableException

  class FunctionNotApplicableException extends RuntimeException

  val aNiceFussyFunction = (x:Int) => x match {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }

  //{1,2,5} => Int

  val aPartialFunction: PartialFunction[Int,Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }//partial function value

  println(aPartialFunction(2))
//  println(aPartialFunction(123123))

  //PF utilities
  println(aPartialFunction.isDefinedAt(67))

  //lift(convert PF to option Total fxn)
  val lifted = aPartialFunction.lift // Int => Option[Int]
  println(lifted(2))
  println(lifted(98))

  //partial fxn kai afnai "orELse"
  val pfChain = aPartialFunction.orElse[Int,Int]{ //orElse implement another partial fxn vanya jastai ho
    case 45 => 67
  }
  println(pfChain(2))
  println(pfChain(45))

  //PR extend normal functions(PF subtype of TF)

  val aTotalFunction: Int => Int = {
    case 1 => 99
  }

  //HOFs accept PF as well
  val aMappedList = List(1,2,3).map{
    case 1 => 42
    case 2 => 78
    case 3 => 1000
  }
  println(aMappedList)

  /*
    Note: PF can only have ONE parameter type
   */

  /**
   * Exercises
   *
   * 1 - construct a PF instance yourself (anonymous class)
   * 2 - dumb chatbot as a PF
   */

  val aManualFussyFunction = new PartialFunction[Int,Int] {
    override def apply(x: Int): Int = x match {
      case 1 => 2
      case 2 => 3
      case 3 => 4
    }
      override def isDefinedAt(x: Int): Boolean =
        x == 1 | x == 2 | x == 3

  }
  val chatBot: PartialFunction[String, String] = {
    case "hello" => "Hi, my name is HAL9000"
    case "goodbye" => "Once you start talking to me, there is no return, human!"
    case "call mom" => "Unable to find your phone without your credit card"
  }

  println(aManualFussyFunction(2))
//  scala.io.Source.stdin.getLines().foreach( _ => println("chatbot says" + chatBot(_)))
  scala.io.Source.stdin.getLines().map(chatBot).foreach(println)


}
