package EShop.lab3

import EShop.lab2.CartActor.{ConfirmCheckoutCancelled, ConfirmCheckoutClosed}
import EShop.lab2.{CartActor, Checkout}
import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class CheckoutTest
  extends TestKit(ActorSystem("CheckoutTest"))
  with AnyFlatSpecLike
  with ImplicitSender
  with BeforeAndAfterAll
  with Matchers
  with ScalaFutures {

  import Checkout._

  override def afterAll: Unit =
    TestKit.shutdownActorSystem(system)

  it should "Send close confirmation to cart" in {
    val cart = TestProbe()
    val orderManager = TestProbe()
    val checkout = system.actorOf(Checkout.props(cart.ref), "checkout1")

    cart.send(checkout, StartCheckout)
    orderManager.send(checkout, SelectDeliveryMethod("deliveryMethod"))
    orderManager.send(checkout, SelectPayment("paymentMethod"))
    orderManager.send(checkout, ConfirmPaymentReceived)
    cart.expectMsg(ConfirmCheckoutClosed)
  }

  it should "be in cancelled state after cancel message received in selectingDelivery State" in {
    val cart = TestProbe()
    val checkout = system.actorOf(Checkout.props(cart.ref), "checkout2")

    cart.send(checkout, StartCheckout)
    cart.send(checkout, CancelCheckout)
    cart.expectMsg(ConfirmCheckoutCancelled)
  }

  it should "be in cancelled state after expire checkout timeout in selectingDelivery state" in {
    val cart = TestProbe()
    val checkout = system.actorOf(Checkout.props(cart.ref), "checkout3")

    cart.send(checkout, StartCheckout)
    Thread.sleep(2000)
    cart.expectMsg(ConfirmCheckoutCancelled)
  }

  it should "be in cancelled state after cancel message received in processingPayment State" in {
    val cart = TestProbe()
    val orderManager = TestProbe()
    val checkout = system.actorOf(Checkout.props(cart.ref), "checkout4")

    cart.send(checkout, StartCheckout)
    orderManager.send(checkout, SelectDeliveryMethod("deliveryMethod"))
    orderManager.send(checkout, SelectPayment("paymentMethod"))
    cart.send(checkout, CancelCheckout)
    cart.expectMsg(ConfirmCheckoutCancelled)
  }

  it should "be in cancelled state after expire checkout timeout in processingPayment state" in {
    val cart = TestProbe()
    val orderManager = TestProbe()
    val checkout = system.actorOf(Checkout.props(cart.ref), "checkout5")

    cart.send(checkout, StartCheckout)
    orderManager.send(checkout, SelectDeliveryMethod("deliveryMethod"))
    orderManager.send(checkout, SelectPayment("paymentMethod"))
    Thread.sleep(2000)
    cart.expectMsg(ConfirmCheckoutCancelled)
  }

  it should "not change state after cancel msg in completed state" in {
    val cart = TestProbe()
    val orderManager = TestProbe()
    val checkout = system.actorOf(Checkout.props(cart.ref), "checkout6")

    cart.send(checkout, StartCheckout)
    orderManager.send(checkout, SelectDeliveryMethod("deliveryMethod"))
    orderManager.send(checkout, SelectPayment("paymentMethod"))
    orderManager.send(checkout, Checkout.ConfirmPaymentReceived)
    cart.expectMsg(CartActor.ConfirmCheckoutClosed)
    cart.send(checkout, CancelCheckout)
    cart.expectNoMessage()
  }
}
