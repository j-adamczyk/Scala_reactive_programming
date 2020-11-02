package EShop.lab3

import EShop.lab2.{Cart, CartActor, CartActorTest}
import akka.actor.{ActorRef, ActorSystem, Cancellable, Props}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class CartActorTest
  extends TestKit(ActorSystem("CartTest"))
  with AnyFlatSpecLike
  with ImplicitSender
  with BeforeAndAfterAll
  with Matchers
  with ScalaFutures {

  import CartActor._
  import CartActorTest._

  override def afterAll: Unit =
    TestKit.shutdownActorSystem(system)

  //use GetItems command which was added to make test easier
  it should "add item properly" in {
    val cart = TestActorRef[CartActor]

    cart ! AddItem("Item")
    cart ! GetItems
    expectMsg(Cart.empty.addItem("Item"))
  }

  it should "be empty after adding and removing the same item" in {
    val cart = TestActorRef[CartActor]

    cart ! AddItem("Item")
    cart ! RemoveItem("Item")
    cart ! GetItems
    expectMsg(Cart.empty)
  }

  it should "contain one item after adding new item and removing not existing one" in {
    val cart = TestActorRef[CartActor]

    cart ! AddItem("Item")
    cart ! GetItems
    expectMsg(Cart.empty.addItem("Item"))

    cart ! RemoveItem("Item2")
    cart ! GetItems
    expectMsg(Cart.empty.addItem("Item"))
  }

  it should "start checkout" in {
    val cart = TestActorRef[CartActor]

    cart ! AddItem("Item")
    cart ! StartCheckout
    expectMsgType[OrderManager.ConfirmCheckoutStarted]
  }

  it should "cancel checkout properly" in {
    val cart = system.actorOf(Props[CartActor])

    cart ! AddItem("Item")
    cart ! StartCheckout
    expectMsgType[OrderManager.ConfirmCheckoutStarted]
    cart ! ConfirmCheckoutCancelled
    cart ! GetItems
    expectMsg(Cart.empty.addItem("Item"))
  }

  it should "close checkout properly" in {
    val cart = system.actorOf(Props[CartActor])

    cart ! AddItem("Item")
    cart ! StartCheckout
    expectMsgType[OrderManager.ConfirmCheckoutStarted]
    cart ! ConfirmCheckoutClosed
    cart ! GetItems
    expectMsg(Cart.empty)
  }

  it should "not add items when in checkout" in {
    val cart = system.actorOf(Props[CartActor])

    cart ! AddItem("Item")
    cart ! StartCheckout
    expectMsgType[OrderManager.ConfirmCheckoutStarted]
    cart ! AddItem("Item2")
    cart ! GetItems
    expectMsg(Cart.empty.addItem("Item"))
  }

  it should "expire and back to empty state after given time" in {
    val cart = system.actorOf(Props[CartActor])

    cart ! AddItem("Item")
    Thread.sleep(1500)
    cart ! AddItem("Item")
    cart ! GetItems
    expectMsg(Cart.empty.addItem("Item"))
  }
}
