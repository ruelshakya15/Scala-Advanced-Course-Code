package lectures.part3concurrency

import scala.collection.mutable
import scala.util.Random

object ThreadCommunication extends App {

  /*
    the producer-consumer problem(producer consumer run in parallel)

    producer -> [ ? ] -> consumer
   */
  class SimpleContainer {
    private var value: Int = 0

    def isEmpty: Boolean = value == 0

    def set(newValue: Int) = value = newValue

    def get = {
      val result = value
      value = 0
      result
    }
  }

  def naiveProdCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[Consumer] waiting ....")
      while (container.isEmpty) {
        println("[Consumer] actively waiting ....")
      }

      println("[consumer] I have consumed " + container.get)
    })

    val producer = new Thread(() => {
      println("[producer] computing .. ")
      Thread.sleep(500)
      val value = 42
      println("[producer] I have produced, after long work, the value " + value)
      container.set(value)
    })

    consumer.start()
    producer.start()
  }

  //  naiveProdCons()

  // wait and notify
  def smartProdCons() = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting ....")
      container.synchronized {
        container.wait()
      }

      // container must have some value
      println("[consumer] I have consumed " + container.get)
    })

    val producer = new Thread(() => {
      println("[producer] Hard at work ...")
      Thread.sleep(2000)
      val value = 42

      container.synchronized {
        println("[producer] I am producing " + value)
        container.set(value)
        container.notify()
      }
    })

    consumer.start()
    producer.start()
  }
  //  smartProdCons()
  //**************************************************************************************************************************************************
  /*
  LEVEL 2:
   producer -> [ ? ? ? ] -> consumer
  */

  def prodConsLargeBuffer() = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 3

    val consumer = new Thread(() => {
      val random = new Random()

      while (true) {
        buffer.synchronized {
          if (buffer.isEmpty) {
            println("[consumer] buffer empty, waiting ...")
            buffer.wait()
          }
          //there must be at lead ONE value in buffer
          val x = buffer.dequeue()
          println("[consumer] consumed " + x)

          // hey producer, there's empty space available, are you lazy?!
          buffer.notify()
        }
        Thread.sleep(random.nextInt(500))
      }
    })

    val producer = new Thread(() => {
      val random = new Random()
      var i = 0

      while (true) {
        buffer.synchronized {
          if (buffer.size == capacity) {
            println("[producer] buffer is full, waiting")
            buffer.wait()
          }
          // there must be at least ONE EMPTY SPACE in the buffer
          println("[producer] producing " + i)
          buffer.enqueue(i)

          //hey consumer, new food for you!
          buffer.notify()

          i += 1
        }
        Thread.sleep(random.nextInt(500))
      }
    })

    consumer.start()
    producer.start()
  }

  //  prodConsLargeBuffer()


  //**************************************************************************************************************************************************
  /*
   Prod-cons, LEVEL 3:

        producer1 ->  [ ? ? ? ] -> consumer1
        producer2 -----^     ^---- consumer2
  */

  class Consumer(id: Int, buffer: mutable.Queue[Int]) extends Thread {
    override def run(): Unit = {
      val random = new Random()

      while (true) {
        buffer.synchronized {
          /*
            producer produces value, two Cons are waiting
            notifies ONE consumer, notifies on buffer
            notifies the other consumer

           */
          while (buffer.isEmpty) { // changed here
            println(s"[consumer $id] buffer empty, waiting ...")
            buffer.wait() //----after this checks again as if -> while so goes to line 161-----^
          }

          //there must be at lead ONE value in buffer
          val x = buffer.dequeue()
          println(s"[consumer $id] consumed " + x)

          buffer.notify()
        }
        Thread.sleep(random.nextInt(500))
      }
    }
  }

  class Producer(id: Int, buffer: mutable.Queue[Int], capacity: Int) extends Thread {
    val random = new Random()
    var i: Int = 0

    override def run(): Unit = {
      while (true) {
        buffer.synchronized {
          while (buffer.size == capacity) {
            println(s"[producer $id] buffer is full, waiting")
            buffer.wait()
          }
          // there must be at least ONE EMPTY SPACE in the buffer
          println(s"[producer $id] producing " + i)
          buffer.enqueue(i)

          //hey consumer, new food for you!
          buffer.notifyAll()

          i += 1
        }
        Thread.sleep(random.nextInt(500))
      }
    }
  }

  def multiProdCons(nConsumers: Int, nProducers: Int) = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]
    val capacity = 20

    (1 to nConsumers).foreach(i => new Consumer(i, buffer).start())
    (1 to nProducers).foreach(i => new Producer(i, buffer, capacity).start())
  }

  // multiProdCons(3, 3)
  //********************************************************************************************************************* New VIDEO
  /*
   Exercises.
   1) think of an example where notifyALL acts in a different way than notify?
   2) create a deadlock
   3) create a livelock
  */

  //  1 - notifyAll
  def testNotifyAll() = {
    val bell = new Object

    (1 to 10).foreach(i => new Thread(() => {
      bell.synchronized {
        println(s"[thread $i] is waiting")
        bell.wait()
        println(s"[thread $i] hooray!")
      }
    }).start())

    new Thread(() => {
      Thread.sleep(2000)
      println(s"[announcer] Rock'n roll!")
      bell.synchronized {
        bell.notifyAll()
      }
    }).start()
  }

  //  testNotifyAll()

  //  2 - deadlock
  // OWN
  //  val obj1 = new Object
  //  val obj2 = new Object
  //
  //  val thread1 = new Thread(() => {
  //    obj1.synchronized{
  //      println("[thread 1] started on obj1")
  //      obj2.synchronized{
  //        println("[thread 1] started on obj2")
  //      }
  //    }
  //  })
  //  val thread2 = new Thread(() => {
  //    obj2.synchronized{
  //      println("[thread 2] started on obj2")
  //      obj1.synchronized{
  //        println("[thread 2] started on obj1")
  //      }
  //    }
  //  })
  //
  //  thread1.start()
  //  thread2.start()

  // Rock the JVM version
  case class Friend(name: String) {
    def bow(other: Friend) =
      this.synchronized {
        println(s"$this : I am bowing to my friend $other")
        other.rise(this) // here we try to lock other friends monitor
        println(s"$this: my friend $other has risen")
      }

    def rise(other: Friend) =
      this.synchronized {
        println(s"$this: I am rising to my friend $other")
      }

    var side = "right"
    def switchSide() = {
      if (side == "right") side = "left"
      else side = "right"
    }

    def pass(other: Friend) =
      while (this.side == other.side){
        println(s"$this: Oh, but please , $other , feel free to pass...")
        switchSide()
        Thread.sleep(1000)
      }
  }

  val sam = Friend("Sam")
  val pierre = Friend("Pierre")

//  new Thread(() => sam.bow(pierre)).start() // sam's locked, attempts to --->       |  then lock pierre's object
//  new Thread(() => pierre.bow(sam)).start() // pierre's locked, attempts to --->   |  then lock sam's object  ; There for deadlock

 //   3 - livelock
  new Thread(() => sam.pass(pierre)).start()
  new Thread(() => pierre.pass(sam)).start()
}
