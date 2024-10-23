package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean){ //extends a function unlike normal collections (kinaki look at apply method)
  /*
    EXERCISE - implement a functional set
   */
  def apply(elem: A): Boolean =
    contains(elem)

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A] // union

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  /*
  EXERCISE #2
  - removing an element
  - intersection with another set
  - difference with another set
 */
  def -(elem: A) : MySet[A] //remove
  def &(anotherSet: MySet[A]): MySet[A] // intersection
  def --(anotherSet: MySet[A]): MySet[A] //difference

  //  EXERCISE #3 implement a unary_! = NEGATION of a set
  // set[1,2,3] => everything but 1,2,3
  // syntactic sugar from beginners course
  def unary_! :MySet[A]
}

class EmptySet[A] extends MySet[A] {
  def contains(elem: A): Boolean = false
  def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)
  def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  def map[B](f: A => B): MySet[B] = new EmptySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  def filter(predicate: A => Boolean): MySet[A] = this
  def foreach(f: A => Unit): Unit = ()

  def -(elem: A) : MySet[A] = this
  def &(anotherSet: MySet[A]): MySet[A] = this
  def --(anotherSet: MySet[A]): MySet[A] = this

  def unary_! :MySet[A] = new PropertyBasedSet[A](_ => true)

}
//  all elements of type A which satisfy a property
// { x in A | property(x) }
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A]{
  def contains(elem: A): Boolean = property(elem)
  // { x in A | property(x) } + element = { x in A | property(x) || x == element }
  def +(elem: A): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || x == elem)
  // { x in A | property(x) } ++ set = { x in A | property(x) || set contains x   }
  def ++(anotherSet: MySet[A]): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || anotherSet(x))

  def map[B](f: A => B): MySet[B] = politelyFail
  def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail
  def foreach(f: A => Unit): Unit = politelyFail

  def filter(predicate: A => Boolean): MySet[A] =new PropertyBasedSet[A](x => property(x) && predicate(x))
  def -(elem: A): MySet[A] = filter(x => x!= elem)
  def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)
  def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)
  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole!")
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A]{

  def contains(elem: A): Boolean = head == elem || tail.contains(elem)
  def +(elem: A): MySet[A] = if (this contains elem) this else new NonEmptySet[A](elem, this)

  /*
    [1 2 3] ++ [4 5] =
    [2 3] ++ [4 5] + 1 =
    [3] ++ [4 5] + 1 + 2 =
    [] ++ [4 5] + 1 + 2 + 3
    [4 5] + 1 + 2 + 3 = [4 5 1 2 3]
   */
  def ++(anotherSet: MySet[A]): MySet[A] =
    tail ++ anotherSet + head

  def map[B](f: A => B): MySet[B] = (tail map f) + f(head)
  def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)
  def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail filter predicate
    if (predicate(head)) filteredTail + head
    else filteredTail
  }
  def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  //part 2
  def -(elem: A) : MySet[A] =
    //OWN implementation
//    if (!this.contains(elem)) this
//    else this.filter(x => x == elem)
    if(head == elem) tail
    else tail - elem + head

  def &(anotherSet: MySet[A]): MySet[A] =  filter(anotherSet) // equivalent to(x => anotherSet.contains(x)) //intersection = filter
//    //OWN 1:
//   this.flatMap{x =>
//     if (anotherSet.contains(x)) MySet(x)
//     else new EmptySet()
//   }
//    //OWN 2:
//    if(!anotherSet.contains(head))  tail & anotherSet
//    else tail & anotherSet + head


  def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)
    //OWN
//    this.flatMap{x =>
//      if (anotherSet.contains(x)) new EmptySet()
//      else MySet(x)
//    }

  //new operator
  def unary_! :MySet[A] = new PropertyBasedSet[A](x => !this.contains(x)) //**Note: contains ko apply method lagcha so new PropertySet ko instance return garda telle predicate fyalcha
}

object MySet { // A* => collection of type A
  /*
    val s = MySet(1,2,3) = buildSet(seq(1,2,3), [])
    = buildSet(seq(2,3), [] + 1)
    = buildSet(seq(3), [1] + 2)
    = buildSet(seq(), [1, 2] + 3)
    = [1,2,3]
   */
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)

    buildSet(values, new EmptySet[A])
  }
}

object MySetPlayground extends App{
  val s = MySet(1,2,3,4)
  val s2 = MySet(2,3)
  s + 5 ++ MySet(-1, -2) + 3 flatMap(x => MySet(x, 10 * x )) filter (_ % 2 == 0) foreach println
  s & s2 foreach println
  s --  s2 foreach println

  val negative = !s // s.unary_! = all the naturals not equal to 1,2,3,4
  println(negative(2))
  println(negative(5))

  val negativeEven = negative.filter(_ % 2 == 0)
  println(negativeEven(5))

  val negativeEven5 = negativeEven + 5 //all the even numbers > 4 + 5
  println(negativeEven5(5))
}



