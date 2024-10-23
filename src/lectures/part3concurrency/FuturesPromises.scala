package lectures.part3concurrency

import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Random, Success, Try}
import scala.concurrent.duration._
// NEED for FUTURES
import scala.concurrent.ExecutionContext.Implicits.global // <--- value used as implicit in FUTURE

object FuturesPromises extends App {

  def calculateMeaningOfLife: Int = {
    Thread.sleep(2000)
    42
  }

  val aFuture = Future {
    calculateMeaningOfLife // calculates the meaning of life on ANOTHER thread
  } // (global) which is passed by the compiler

  println(aFuture.value) // Option[Try[Int]]

  println("Waiting on the future")

  aFuture.onComplete { //"onComplete" returns (Unit) so used for sideEffects
    case Success(meaningOfLife) => println(s"the meaning of life is $meaningOfLife")
    case Failure(e) => println(s"I have failed with $e")
  } // SOME thread executes it
  Thread.sleep(3000) // so MAIN thread doesnt end before future completes

  // ****************************************************** 2ND VIDEO ******************************************************************************
  // mini social network

  case class Profile(id: String, name: String) {
    def poke(anotherProfile: Profile) =
      println(s"${this.name} poking ${anotherProfile.name}")
  }

  object SocialNetwork {
    //"database"
    val names = Map(
      "fb.id.1-zuck" -> "Mark",
      "fb.id.2-bill" -> "Bill",
      "fb.id.3-dummy" -> "Dummy",
    )

    val friends = Map(
      "fb.id.1-zuck" -> "fb.id.2-bill"
    )
    val random = new Random()

    // API
    def fetchProfile(id: String): Future[Profile] = Future {
      Thread.sleep(random.nextInt(300))
      Profile(id, names(id))
    }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(400))
      val bfId = friends(profile.id)
      Profile(bfId, names(bfId))
    }
  }

  // client: mark to poke bill
  val mark = SocialNetwork.fetchProfile("fb.id.1-zuck")

  //  **[FROM SCALA 3]
  def pokeBestFriend(accountId: String): Unit = {
    // 1 - call fetchProfile
    // 2 - call fetchBestFriend
    // 3 - call profile.poke(bestFriend)
    val profileFuture = SocialNetwork.fetchProfile(accountId)
    profileFuture.onComplete {
      case Success(profile) => // "code block"                                          //THIS CODE NOT NICE --------------- BELOW FOR COMPREHENSION BETTER
        val friendProfileFuture = SocialNetwork.fetchBestFriend(profile)
        friendProfileFuture.onComplete {
          case Success(friendProfile) => profile.poke(friendProfile)
          case Failure(e) => e.printStackTrace()
        }
      case Failure(ex) => ex.printStackTrace()
    }
  } // onComplete is such a pain - callback hell!


  // onComplete is a hassle
  // SOLUTION : functional composition of futures (TO WRITE CLEANER CODE THAN ABOVE)(CAN manipulate value in FUTURE W/O "onComplete")
  //map, flatMap, filter
  val nameOnTheWall: Future[String] = mark.map(profile => profile.name) // map transforms value contained inside, ASYNCHRONOUSLY
  val marksBestFriend: Future[Profile] = mark.flatMap(profile => SocialNetwork.fetchBestFriend(profile))
  val zucksBestFriendRestricted: Future[Profile] = marksBestFriend.filter(profile => profile.name.startsWith("Z"))

  def pokeBestFriend_v2(accountId: String): Unit = {
    val profileFuture = SocialNetwork.fetchProfile(accountId)
    val action = profileFuture.flatMap { profile => // Future[Unit]
      SocialNetwork.fetchBestFriend(profile).map { bestFriend => // Future[Unit]
        profile.poke(bestFriend) // unit
      }
    }
  }

  //for-comprehensions (MUCH EASIER to read than ABOVE) V3
  def pokeBestFriend_v3(accountId: String): Unit =
    for {
      profile <- SocialNetwork.fetchProfile(accountId) // RECOMMENDED APPROACH to use in FUTURES (THIS AND flatMap,map,filter)
      bestFriend <- SocialNetwork.fetchBestFriend(profile)
    } yield profile.poke(bestFriend) // identical to v2

  // Different v1,v2,v3 called
  //  pokeBestFriend("fb.id.1-zuck")
  //  pokeBestFriend_v2("fb.id.1-zuck")
  pokeBestFriend_v3("fb.id.1-zuck")

  Thread.sleep(1000)

  //  fallbacks
  val aProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recover { // partial fxn ("recover" returns anything)
    case e: Throwable => Profile("fb.id.0-dummy", "Forever alone")
  }

  val aFetchedProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id").recoverWith { //("recoverWith" returns Future)
    case e: Throwable => SocialNetwork.fetchProfile("fb.id.0-dummy")
  }

  val fallbackresult = SocialNetwork.fetchProfile("unknow id").fallbackTo(SocialNetwork.fetchProfile("fb.id.0-dummy")) // if 1st future fails 2nd future value used(if BOTH fail exception of 1st thrown)

  // *******************************************************************3RD VIDEO "PROMISES"********************************************************
  // online banking app
  case class User(name: String)

  case class Transaction(sender: String, receiver: String, amount: Double, status: String)

  object BankingApp {
    val name = "Rock the JVM banking"

    def fetchUser(name: String): Future[User] = Future {
      // simulate fetching from the DB
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] = Future {
      // simulate some processes
      Thread.sleep(1000)
      Transaction(user.name, merchantName, amount, "SUCCESS")
    }

    def purchase(username: String, item: String, merchantName: String, cost: Double): String = {
      /*
       1. fetch user
       2. create transaction
       3. WAIT for the txn to finish
      */
      val transactionStatusFuture = for {
        user <- fetchUser(username)
        transaction <- createTransaction(user, merchantName, cost)
      } yield transaction.status

      // blocking call
      Await.result(transactionStatusFuture, 2.seconds) // implicit conversion -> pimp my library //** Await.ready => return Future[U]
    }
  }

  println(BankingApp.purchase("Daniel", "iPhone12", "rock the jvm store", 3000))

  // promises

  val promise = Promise[Int]() // "controller" over a future                  //*** This Section IMP to understand look at future Part 4 last 25:00 to understand as well
  val future: Future[Int] = promise.future // <--------------------------------------------------------------------|
//                                                                                                                 |
  // thread 1 -"consumer"                                                                                          |
  future.onComplete {                                               //                                             |
    case Success(r) => println("[consumer] I've received " + r)   // this part triggered once promise fulfilled as | this future is part of the promise
    case Failure(ex) => ex.printStackTrace()
  }

  // thread 2 - "producer"
  val producer = new Thread(() => {
    println("[producer] crunching numbers")
    Thread.sleep(500)
    // "fulfilling" the promise
    promise.success(42) // OR, promise.fail(0) in cases of failure
    println("[producer] done")
  })

  producer.start()
  Thread.sleep(1000)

  /*
  EXERCISE
   1) fulfill a future IMMEDIATELY with a value
   2) inSequence(fa, fb)
   3) first(fa, fb) => new future with the first value of the two futures                 *** ALL EXERCISES IMP TO UNDERSTAND
   4) last(fa, fb) => new future with the last value
   5) retryUntil[T](action: () => Future[T], condition: T => Boolean): Future[T]
  */

  //  1 - fulfill immediately
  def fulfillImmediately[T](value: T): Future[T] = Future(value)

  // 2 - inSequence
  def inSequence[A, B](first: Future[A], second: Future[B]): Future[B] = first.flatMap(_ => second) // first completed and then only second returned

  // 3 - first out of two futures // *** LOOK at this exercise(also listen to video)
  def first[A](fa: Future[A], fb: Future[A]): Future[A] = {
    val promise = Promise[A]
    fa.onComplete(x => promise.tryComplete(x)) // expanded form esto huncha(Try[] ko instance sending to .tryComplete() to check
    fb.onComplete(promise.tryComplete) //if "fa" completes first this statement does NOTHING

    promise.future

    //  Below does same as Above

    //    def tryComplete(promise: Promise[A], result: Try[A]) = result match {
    //      case Success(r) => try {    // *** pass garne "Try" diff from this "try" so first ma (Success/Fail) sanga case match pachi chai java "try" sanga
    //        promise.success(r)
    //      } catch {
    //        case _ =>
    //      }
    //      case Failure(e) => try {
    //        promise.failure(e)
    //      } catch {
    //        case _ =>
    //      }
    //    }
    //    fa.onComplete(result => tryComplete(promise, result )) //onComplete returns Try[A]
    //    fb.onComplete(tryComplete(promise, _ )) // syntactic sugar

  }

  //  4 - last out of the 2 futures
  def last[A](fa: Future[A], fb: Future[A]): Future[A] = {
    // 1 promise which both futures will try to complete
    // 2 promise which the LAST future will complete
    val bothPromise = Promise[A]
    val lastPromise = Promise[A]
    val checkAndComplete =  (result: Try[A]) =>{
      if(!bothPromise.tryComplete(result))  // if promise completed by other Future => lastpromise complete garne [NAVAYA "tryComplete" call gardai complete huncha]
        lastPromise.complete(result)
    }

    fa.onComplete(checkAndComplete)
    fb.onComplete(checkAndComplete)

    lastPromise.future
  }



  val fast = Future {
    Thread.sleep(100)
    42
  }

  val slow = Future {
    Thread.sleep(200)
    45
  }

  first(fast, slow).foreach(f => println("FIRST " + f))
  last(fast, slow).foreach(f => println("LAST " + f))

  Thread.sleep(1000)

  // 5 - retry until

  def retryUntil[A](action: () => Future[A], condition: A => Boolean): Future[A] =
    action()
      .filter(condition)
      .recoverWith{
        case _ => retryUntil(action, condition)
      }

  val random = new Random()
  val action = () => Future{
    Thread.sleep(100)
    val nextValue = random.nextInt(100)
    println("generated " + nextValue)
    nextValue
  }

  retryUntil(action, (x: Int) => x < 10).foreach(result => println("Settled at " + result))
  Thread.sleep(10000)
}
