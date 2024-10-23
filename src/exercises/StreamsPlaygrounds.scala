package exercises

import scala.annotation.tailrec

abstract class MyStream[+A]{
  def isEmpty: Boolean
  def head: A
  def tail: MyStream[A]

  def #::[B >: A](element:B) : MyStream[B] //prepend operator
  def ++[B >: A](anotherStream: => MyStream[B]):MyStream[B] // concatenate 2 streams

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter(predicate: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A] // takes first n elements out of this stream
  def takeAsList(n: Int): List[A] = take(n).toList()

  /*
    [1, 2, 3].toList([]) =
    [2, 3].toList([1]) =
    [ 3].toList([2 1 ]) =
    [].toList([3 2 1 ]) =
      = [1 2 3]

   */
  @tailrec  //**** See implementation of this
  final def toList[B >: A](acc: List[B] = Nil): List[B] =
    if (isEmpty) acc.reverse
    else tail.toList(head :: acc)
}

object EmptyStream extends MyStream[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException()
  def tail: MyStream[Nothing] = throw new NoSuchElementException()
  def #::[B >: Nothing](element: B): MyStream[B] = new Cons(element, this)
  def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

  def foreach(f: Nothing => Unit): Unit = ()
  def map[B](f: Nothing => B): MyStream[B] = this
  def flatMap[B](f: Nothing=> MyStream[B]): MyStream[B] = this
  def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this
  def take(n: Int): MyStream[Nothing] = this

}

class Cons[+A](hd: A, tl: => MyStream[A]) extends MyStream[A]{ //** MAIN LOGIC: tail part "called by name" so lazily evaluated
  def isEmpty: Boolean = false

  override val head: A = hd
  override lazy val tail: MyStream[A] = tl // call by need

  /*
    Working of below:
    val s = new Cons(1, EmptyStream) "EmptyStream" part is lazy evaluated
    val prepended = 1 #:: s = new Cons(1, s) here "s" is also lazy evaluated
   */
  def #::[B >: A](element: B): MyStream[B] = new Cons(element, this)
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = new Cons(head, tail ++ anotherStream) // ****NOTICE : we are using "head"/"tail" instead of hd/tl as they are lazy evaluated

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }
  /*
  Suppose:
    s = new Cons(1, ?)
    mapped = s.map(_ + 1) = new Cons(2, s.tail.map(_ + 1))
    ..... mapped.tail  //<------- unless we call this,  "s.tail.map(_ + 1)" THIS part wont be evaluated  (ONLY evaluated in a by need basis)
   */
  def map[B](f: A => B): MyStream[B] = new Cons(f(head), tail.map(f))
  def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)
  def filter(predicate: A => Boolean): MyStream[A] =
    if (predicate(head)) new Cons(head, tail.filter(predicate))
    else tail.filter(predicate) // preserve lazy eval as at most 1 Element evaluated

  def take(n: Int): MyStream[A] =
    if(n == 0 ) EmptyStream
    else if(n == 1)  new Cons(head, EmptyStream)
    else new Cons(head, tail.take(n - 1))

}

object MyStream{
  def from[A](start: => A)(generator : A => A): MyStream[A] =
    new Cons(start, MyStream.from(generator(start))(generator)) //  AGAIN this part "MyStream.from(generator(start))(generator)" LAZY evaluated
}

object StreamPlaygrounds extends App{
  val naturals = MyStream.from(1)(_ + 1)
  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)

  val startFrom0 = 0 #:: naturals // rewritten: naturals.#::(0) right association
  println(startFrom0.head)

  startFrom0.take(1000).foreach(println)

  //map, flatMap
  println(startFrom0.map(_ * 2).take(100).toList())
  println(startFrom0.flatMap(x => new Cons(x, new Cons(x + 1, EmptyStream))).take(10).toList())
  println(startFrom0.filter(_ < 10 ).take(10).take(20).toList())//how program not crash with .filter.take? => see line 59-63(LazyEvaluation) 1 < 30 filter huncha ani tei 1 < 20 so (either this or startfrom0 jasari stored huncha lazily .map haru garda ni tesari stored huncha)

  // Exercises on streams
  // 1 - stream of Fibonacci numbers
  // 2 - stream of prime numbers with Eratosthenes' sieve
  /*
    [ 2 3 4 ... ]
    filter out all numbers divisible by 2
    [ 2 3 5 7 9 11 ...]
    filter  out all numbers divisible by 3
    [ 2 3 5 7 11 13 17 ... ]
    filter out all numbers divisible by 5
      ...
   */

  //  1
  def fibonacci(first: BigInt, second: BigInt): MyStream[BigInt] =
    new Cons(first, fibonacci(second, first + second))

  println(fibonacci(1,1).take(20).toList())

  // 2

  /*
    [ 2 3 4 5 6 7 8 9 10 11 12 ...
    [ 2 3 5 7 9 11 13 ...
    [ 2 eratosthenes applied to (numbers filtered by n % 2 != 0)
    [ 2 3 eratosthenes applied to [ 5 7 9 11 ... ] filtered by n % 3 != 0
    [ 2 3 5
   */

  def eratosthenes(numbers: MyStream[Int]): MyStream[Int] = {
    if (numbers.isEmpty) numbers
    else new Cons(numbers.head, eratosthenes(numbers.filter(_ % numbers.head != 0 )))
  }

  println(eratosthenes(MyStream.from(2)(_ + 1)).take(100).toList())
  Some(4).flatMap(x =>Some(x*2))

}