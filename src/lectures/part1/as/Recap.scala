package lectures.part1.as

import scala.annotation.tailrec

object Recap extends App {

  val aCondition: Boolean = false
  val aConditionedVal = if (aCondition) 42 else 45
  //instruction vs expressions

  //compiler infers types for us
  val aCodeBlock = {
    if (aCondition) 54
    56
  }

  //Unit = void
  val theUnit = println("hello, Scala")

  //functions
  def aFunction(x: Int): Int = x + 1

  //recursion: Stack and tail
  @tailrec def factorial(n: Int, accumulator: Int): Int =
    if (n<= 0) accumulator
    else factorial(n - 1, accumulator * n)

  // OOP

  class Animal
  class Dog extends Animal
  val aDog: Animal = new Dog // subtyping polymorphism

  trait Carnivore {
    def eat(a: Animal): Unit
  }
  class Crocodile extends Animal with Carnivore{
    override def eat(a: Animal): Unit = println("crunch!")
  }

  //method notations
  val aCroc = new Crocodile
  aCroc.eat(aDog)
  aCroc eat aDog // natural language

  // anonymous classes
  val aCarnivore = new Carnivore {
    override def eat(a: Animal): Unit = println("roar!")
  }

  // generics
  abstract class Mylist[+A] // variance & variance problems
  // singleton objects and companions
  object Mylist

  // case classes
  case class Person(name: String, age: Int)

  //exception and try/catch/finally

  val throwsException = throw new RuntimeException() //Nothing
  val aPotentialFailure = try{
    throw new RuntimeException()
  }catch {
    case e: Exception => "I caught an exception"
  }finally {
    println("some logs")
  }

  //packaging and imports

  // functional programming
  val incrementer = new Function[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }
  incrementer(1)

  val anonymousIncrementer = (x: Int) => x + 1
  List(1,2,3).map(anonymousIncrementer) // HOF
  //amp, flatMap, filter

  //for -comprehenstion
  val pairs = for{
    num <- List(1,2,3) // if condition
    char <- List('a','b','c')
  }yield num + "-" + char

  // Scala collections: Seqs, Arrays, List, Vector, Maps, Tuples
  val aMap = Map(
    "Daniel" -> 789,
    "jess" -> 555
  )

  //"collection": Options, Try
  val anOption = Some(2)

  // pattern matching
  val x = 2
  val order = x match {
    case 1 => "first"
    case 2 => "second"
    case 3 =>"third"
    case _ => x + "th"
  }

  val bob = Person("Bob", 22)
  val greeting = bob match {
    case Person(n, _) => s"Hi, my name is $n"
  }

  // all the patterns
}
