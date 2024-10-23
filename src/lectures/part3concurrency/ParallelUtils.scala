package lectures.part3concurrency

// NEEDED FOR ".par"

import java.util.concurrent.ForkJoinPool
import java.util.concurrent.atomic.{AtomicReference, AtomicReferenceArray}
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.{ForkJoinTaskSupport, Task, TaskSupport}
import scala.collection.parallel.immutable.ParVector

object ParallelUtils extends App {

  // 1 - parallel collections(2 ways)

  val parList = List(1, 2, 3).par

  val aParVector = ParVector[Int](1, 2, 3)

  /*
    Seq
    Vector
    Array
    Map - Hash, Trie
    Set - Hash, Trie
   */

  def measure[T](operation: => T): Long = {
    val time = System.currentTimeMillis()
    operation // forced to evaluate
    System.currentTimeMillis() - time
  }

  val list = (1 to 10000).toList
  val serialTime = measure {
    list.map(_ + 1)
  }
  println("serial time: " + serialTime) // *** NOTE: for 1mil parallel collection faster for 10k serial faster so only use in large collections

  val parallelTime = measure {
    list.par.map(_ + 1)
  }
  println("parallel time: " + parallelTime)

  /*
  PARALLEL COLLECTION WORKS ON :
    Map-reduce model
    - split the elements into chunks - Splitter (process each chunk parallely processed by separate threads)
    - operation
    - recombine - Combiner
   */

  // [map, flatMap, filter, foreach] => (SAFE) ,[reduce, fold] => NOT ALWAYS SAFE

  // BE CAREFUL with "fold(),reduce()" with non - associative operators // dont know how element will be brought together
  println(List(1, 2, 3).reduce(_ - _))
  println(List(1, 2, 3).par.reduce(_ - _))

  // synchronization(CAREFUL with foreach() too)
  var sum = 0
  List(1, 2, 3).par.foreach(sum += _) // 6 NOT GUARANTEED [Race conditions may occur]
  println(sum)

  // configuring ( because we dnt know how thread are managed in parallel collections) [2 ways]
    // 1 way
  aParVector.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(2)) // no of thread that will manage aParVector
  /*
    alternatives
    - ThreadPoolTaskSupport - deprecated
    - ExecutionContextTaskSupport(EC)
   */

   //2 way
  aParVector.tasksupport = new TaskSupport {
    override def execute[R, Tp](fjtask: Task[R, Tp]): () => R = ???

    override def executeAndWaitResult[R, Tp](task: Task[R, Tp]): R = ???

    override def parallelismLevel: Int = ???

    override val environment: AnyRef = ???
  }

  // 2 - atomic ops and references

  val atomic = new AtomicReference[Int](2) // normal Int variable lai thread-safe way ma use garne

  val currentValue = atomic.get() // thread -safe read [no race conditions testo kura]
  atomic.set(4) // thread-safe write

  atomic.getAndSet(5) // thread safe combo

  atomic.compareAndSet(38, 56) // if value == 37, then set it to 56
  // reference equality

  atomic.updateAndGet(_ + 1) // like .map
  atomic.getAndUpdate(_ + 1) // (reverse of above) gets and then runs the function

  atomic.accumulateAndGet(12, _ + _) // thread-safe accumulation
  atomic.getAndAccumulate(12, _ + _) //(reverse of above)

}
