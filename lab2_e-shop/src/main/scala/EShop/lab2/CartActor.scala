package EShop.lab2

import akka.actor.{Actor, ActorRef, Cancellable, Props, Timers}
import akka.event.{Logging, LoggingReceive}

import scala.concurrent.duration._
import scala.language.postfixOps

object CartActor {
  sealed trait Command
  case class AddItem(item: Any) extends Command
  case class RemoveItem(item: Any) extends Command
  case object ExpireCart extends Command
  case object StartCheckout extends Command
  case object ConfirmCheckoutCancelled extends Command
  case object ConfirmCheckoutClosed extends Command

  sealed trait Event
  case class CheckoutStarted(checkoutRef: ActorRef) extends Event

  def props = Props(new CartActor())
}

class CartActor extends Actor with Timers {
  import context._
  import CartActor._

  private val log = Logging(context.system, this)
  val cartTimerDuration: FiniteDuration = 5 seconds

  private def scheduleTimer: Cancellable =
    system.scheduler.scheduleOnce(cartTimerDuration, self, ExpireCart)

  def receive: Receive = empty

  def empty: Receive = {
    case AddItem(item) =>
      context become nonEmpty(Cart.empty.addItem(item), scheduleTimer)

    case "print" =>
      println("Current cart: empty")
  }

  def nonEmpty(cart: Cart, timer: Cancellable): Receive = {
    case AddItem(item) =>
      timer.cancel()
      context become nonEmpty(cart.addItem(item), scheduleTimer)

    case RemoveItem(item) if cart.contains(item) && cart.size == 1 =>
      timer.cancel()
      context become empty
    case RemoveItem(item) if cart.contains(item) =>
      timer.cancel()
      context become nonEmpty(cart.removeItem(item), scheduleTimer)

    case StartCheckout =>
      timer.cancel()
      context become inCheckout(cart)

    case ExpireCart =>
      timer.cancel()
      println("Cart expired")
      context become empty

    case "print" =>
      println("Current cart: " + cart)
  }

  def inCheckout(cart: Cart): Receive = {
    case ConfirmCheckoutCancelled =>
      context become nonEmpty(cart, scheduleTimer)

    case ConfirmCheckoutClosed =>
      println("Cart closed after checkout")
      context become empty

    case "print" =>
      println("Current cart: " + cart)
  }
}
