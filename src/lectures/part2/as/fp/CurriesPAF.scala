package lectures.part2.as.fp

object CurriesPAF extends App {
  //curried functions
  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3 = superAdder(3) // Int => Int = y => 3 + y
  println(add3(5))
  println(superAdder(5)(3))

  // METHOD !
  def curriedAdder(x: Int)(y: Int): Int = x + y //curried method


  val add4: Int => Int = curriedAdder(4) // need declaration here(for METHOD) converted [METHOD -> Function]
  // cannot pass METHOD to HOF (as Method are instances of classes in JVM)

  //  lifting = ETA-EXPANSION
  //function != methods(JVM limitations)
  def inc(x: Int) = x + 1
  List(1,2,3).map(inc) // Bts: compiler does ETA-expansion  List(1,2,3).map(inc) -> List(1,2,3).map(x => inc(x))

  // Partial function applications
  val add5 = curriedAdder(5) _ // tells compiler to perform ETA-EXPANSION convert -> Int => Int

  // EXERCISE
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)(y: Int) = x + y

  // add7: Int => Int = y => 7 + y
  // as many different implementations of add7 using the above
  // be creative!

  val add7 = (x: Int) => simpleAddFunction(7, x) // simplest
  val add7_2 = simpleAddFunction.curried(7) // change normal method to curried

  val add7_3 = curriedAddMethod(7) _ // PAF
  val add7_4 = curriedAddMethod(7)(_) // PAF == alternate syntax

  val add7_5 = simpleAddMethod(7, _ : Int) // alternate syntax methods -> fxn values
        // rewritten: y => simpleAddMethod(7,y
  val add7_6 = simpleAddFunction(7, _: Int) //same with fxns

  //underscore are powerful
  def concatenator(a: String, b: String, c: String) = a + b + c
  val insertName = concatenator("Hello, I'm ", _: String, ",how are you ?") // x: String => concatenator(hello, x, howarewyou)
  println(insertName("Tom"))

  val fillInTheBlanks = concatenator("Hello, ", _: String, _: String) // (x, y) => concatenator("Hello, ", x, y)
  println(fillInTheBlanks("Tom ","Scala is awesome!"))

  // EXERCISES
  /*
    1.  Process a list of numbers and return their string representations with different formats
        Use the %4.2f, %8.6f and %14.12f with a curried formatter function.
   */

  def curriedFormatter(s: String)(number: Double): String = s.format(number)
  val numbers = List(Math.PI, Math.E, 1, 9.8, 1.3e-12)
  val simpleFormat = curriedFormatter("%4.2f") _ // lift
  val seriousFormat = curriedFormatter("%8.6f") _
  val preciseFormat = curriedFormatter("%14.12f") _

  println(numbers.map(preciseFormat))

  /*
    2.  difference between
        - functions vs methods
        - parameters: by-name vs 0-lambda
   */
  def byName(n: => Int) = n + 1
  def byFunction(f: () => Int) = f() + 1

  def method: Int = 42
  def parenMethod(): Int = 42

  /*
    calling byName and byFunction
    - int
    - method
    - parenMethod
    - lambda
    - PAF
   */
  byName(23) //ok
  byName(method) //ok
  byName(parenMethod()) //ok
  byName(parenMethod) //Scala 2: ok but beware ==> byName(parenMethod()); Scala 3 forbids calling the method with no parens
//  byName(() => 42)//not ok
  byName((()=> 42)()) /// ok (also calling the fxn like ()
//  byName(parenMethod _) //not ok

//  byFunction(45) // not ok
//  byFunction(method) // not ok !!! does not do ETA
  byFunction(parenMethod) //does ETA -expansion
  byFunction(() => 46) // ok
  byFunction(parenMethod _) //also ok
}
