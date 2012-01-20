import akka.actor.{PoisonPill, ActorRef, Actor}
import System._
import Integer._

object Ring {

  class Node(source: ActorRef, number: Int) extends Actor{

    protected def receive = {
      case 'start =>
        source ! 'ping
        if (number == 1)  {
          source ! 'ok
          self ! PoisonPill
        } else {
          Node(source, number - 1)  ! 'ok
        }
      case 'ok =>
        self.stop
    }
  }

  object Node {

    def apply (source: ActorRef, number: Int) = {
      val actor: ActorRef = Actor.actorOf(new Node(source, number)).start()
      actor ! 'start
      actor
    }
  }

  class Source(number: Int, maximum: Int) extends Actor{
    var start: Long = _
    var total: Int = _

    var counter = maximum

    override def preStart() {
      start = currentTimeMillis()
      Node(self, number) ! 'ok
    }

    override def postStop() {
    }

    def decreaseCounter () {
      counter = counter - 1
    }

    protected def receive = {
      case 'ping => total = total + 1
      case 'ok =>
        println(currentTimeMillis() - start + "ms for " + total)
        decreaseCounter()
      if (counter != 0) {
        println("Retrying... " + counter + " times")
        start = currentTimeMillis()
        total = 0
        Node(self, number) ! 'ok
      } else {
         self.stop
      }
    }
  }

  object Source {
    def apply (number: Int, maximum: Int) = Actor.actorOf(new Source(number, maximum)).start()
  }

  def  main(arguments: Array[String]) {
//    Thread.sleep(5000)
    Source(parseInt(arguments(0)), parseInt(arguments(1)))
 }

}