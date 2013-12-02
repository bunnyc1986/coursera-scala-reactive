package nodescala

import scala.language.postfixOps
import scala.util.{ Try, Success, Failure }
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{ async, await }
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.util.NoSuchElementException

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("all happy case") {
    val always517 = Future.always(517)
    val alway999 = Future.always(999)
    val allresults = Await.result(Future.all(List(always517, alway999)), 1 second)

    assert(allresults(0) == 517)
    assert(allresults(1) == 999)
  }

  test("all with one failure") {
    val always517 = Future.always(517)
    val alway999 = Future.always(999)
    val failure = Future { new RuntimeException }
    val f = Future.all(List(always517, failure, alway999))
    try {
      Await.result(f, 1 second)
      assert(false)
    } catch {
      case t: RuntimeException =>
    }
  }

  test("any happy case") {
    val always517 = Future.always(517)
    val never = Future.never
    val anyresults = Await.result(Future.any(List(never, always517)), 1 second)

    assert(anyresults == 517)
  }

  test("delay") {
    val start = System.currentTimeMillis()
    Await.result(Future.delay(1 second), 2 second)
    val duration = System.currentTimeMillis() - start
    assert(duration < 1005)
  }

  test("now happy case") {
    val alway999 = Future.always(999)
    assert(alway999.now == 999)
  }

  test("now not complete") {
    val delayed999 = Future {
      Thread.sleep(500)
      999
    }
    try {
      delayed999.now
      assert(false)
    } catch {
      case t: NoSuchElementException =>
    }
  }

  test("now throw exception") {
    val throwException = Future {
      throw new RuntimeException
    }
    try {
      throwException.now
      assert(false)
    } catch {
      case t: RuntimeException =>
    }
  }

  test("continueWith") {
    val f = Future[Int] {
      Thread.sleep(500)
      100
    }
    val cont = f.continueWith(f => f.now + 50)
    val result = Await.result(cont, 1 second)
    assert(result == 150)
  }

  test("continue") {
    val f = Future[Int] {
      Thread.sleep(500)
      100
    }
    val cont = f.continue(f => f.get + 50)
    val result = Await.result(cont, 1 second)
    assert(result == 150)
  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




