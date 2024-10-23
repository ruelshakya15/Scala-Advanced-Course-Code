package lectures.part2.as.fp

import lectures.part1.as.AdvancedPatternMatching.Empty

import scala.annotation.tailrec

object LazyEvaluation extends App {

  //lazy DELAYS the eval of values
  lazy val x: Int = {
    println("hello")
    42
  }
  println(x)
  println(x) // hello only printed once *** IMP Understand

  //eg of implications:
  def sideEffectCondition: Boolean = {
    println("Boo") // not printed
    true
  }
  def simpleCondition: Boolean = false

  lazy val lazyCondition = sideEffectCondition
  println(if (simpleCondition && lazyCondition) "yes" else "no") // since simpleCondition false  &&... not evaluated so "boo" not printed

  //  in conjunction with CBN
  def byNameMethod(n: => Int): Int ={
    // (CALL BY NEED)
    lazy val t =n //only evaluated once
    t + t + t + 1
  }
  def retrieveMagicValue = {
    //side effect or long computation
    println("waiting")
    Thread.sleep(1000)
    42
  }

  println(byNameMethod(retrieveMagicValue))
  //use lazy val (CALL BY NEED) "waiting" doesnt print 3 times

  // filtering with lazy vals
  def lessThan30(i : Int): Boolean = {
    println(s"$i is less than 30?")
    i < 30
  }

  def greaterThan20(i : Int): Boolean = {
    println(s"$i is greater than 20?")
    i > 20
  }

  val numbers = List(1,25,40,5,23)
  val lt30 = numbers.filter(lessThan30) // List(1,25,5,23)
  val gt20 = lt30.filter(greaterThan20)
  println(gt20)

  val lt30lazy = numbers.withFilter(lessThan30) //"withFilter" uses lazy values
  val gt20lazy = lt30lazy.withFilter(greaterThan20)
  println
//  println(gt20lazy)
  gt20lazy.foreach(println) // eval no on by need basis look at console

  // for-comprehension use withFilter with guards
  for{
    a <- List(1,2,3) if a % 2 == 0 //"if guards" use lazy val
  }yield a + 1

  //above equivalent to:
  List(1,2,3).withFilter(_% 2 == 0).map(_ + 1) // List[Int]

  /*
    Exercise: implement a lazily evaluated, singly linked STREAM of elements.

    naturals = MyStream.from(1)(x => x + 1) = stream of natural numbers (potentially infinite!)
    naturals.take(100).foreach(println) // lazily evaluated stream of the first 100 naturals (finite stream)
    naturals.foreach(println) // will crash - infinite!
    naturals.map(_ * 2) // stream of all even numbers (potentially infinite)
   */



}
