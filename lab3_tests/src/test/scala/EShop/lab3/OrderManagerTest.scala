package EShop.lab3

import EShop.lab3.OrderManager._
import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class OrderManagerTest
  extends TestKit(ActorSystem("CartTest"))
  with AnyFlatSpecLike
  with ImplicitSender
  with BeforeAndAfterAll
  with Matchers
  with ScalaFutures {

  override def afterAll: Unit =
    TestKit.shutdownActorSystem(system)

  it should "work" in {
    val orderManager = system.actorOf(Props[OrderManager])

    orderManager ! AddItem("Item")
    expectMsg(Done)
    orderManager ! Buy
    expectMsg(Done)
    orderManager ! SelectDeliveryAndPaymentMethod("paypal", "inpost")
    expectMsg(Done)
    orderManager ! Pay
    expectMsg(Done)
    orderManager ! Pay
    expectMsg("order manager finished job")
    expectMsg(Done)
  }
}
