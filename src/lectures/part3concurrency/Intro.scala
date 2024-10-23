package lectures.part3concurrency

import org.omg.CORBA.portable.Delegate

import java.util.concurrent.Executors

object Intro extends App{
  /*
  interface Runnable {
    public void run()
  }
   */
  //  JVM threads
  val runnable = new Runnable {
    def run(): Unit = println("Runnin in parallel")
  }
  val aThread = new Thread(runnable)

//  aThread.start() // gives the signal to the JVM to start a JVM thread
  // creat a JVM thread => OS thread
  runnable.run() // doesn't do anything in parallel
  aThread.join() // blocks until aThread finishes running

  val threadHello = new Thread(() => (1 to 10).foreach(_ => println("hello")))
  val threadGoodbye = new Thread(() => (1 to 10).foreach(_ => println("goodbye")))
//  threadHello.start()
//  threadGoodbye.start()
  // different runs produce different results!

  // executors( since threads are expensive we use"executors" to reuse them)
  val pool = Executors.newFixedThreadPool(10)
//  pool.execute(() => println("something in the thread pool"))

//  pool.execute(() => {
//    Thread.sleep(1000)
//    println("done after 1 second") // this
//  })
//
//  pool.execute(() => {
//    Thread.sleep(1000)
//    println("almost done") // & this printed at same time "pool" executes in multiple threads
//    Thread.sleep(1000)
//    println("done after 2 seconds")
//  })

  pool.shutdown() // after shutdown pool doesn't accept anymore actions
//  pool.execute(() => println("should not appear")) // doesnt print as pool shutdown ( throws an exception in calling thread)

//  pool.shutdownNow() // interrupt running(sleeping) threads
  println(pool.isShutdown) // true (even if threads are running)


}
