package EShop.lab2

import EShop.lab2.Checkout._
import akka.actor.{Actor, ActorRef, Cancellable, Props}
import akka.event.{Logging, LoggingReceive}

import scala.concurrent.duration._
import scala.language.postfixOps

object Checkout {
  sealed trait Data
  case object Uninitialized extends Data
  case class SelectingDeliveryStarted(timer: Cancellable) extends Data
  case class ProcessingPaymentStarted(timer: Cancellable) extends Data

  sealed trait Command
  case object StartCheckout extends Command
  case class SelectDeliveryMethod(method: String) extends Command
  case object CancelCheckout extends Command
  case object ExpireCheckout extends Command
  case class SelectPayment(payment: String) extends Command
  case object ExpirePayment extends Command
  case object ConfirmPaymentReceived extends Command

  sealed trait Event
  case object CheckOutClosed extends Event
  case class PaymentStarted(payment: ActorRef) extends Event

  def props(cart: ActorRef) = Props(new Checkout())
}

class Checkout extends Actor {
  import context._

  private val scheduler = context.system.scheduler
  private val log = Logging(context.system, this)

  val checkoutTimerDuration: FiniteDuration = 5 seconds
  val paymentTimerDuration: FiniteDuration = 5 seconds

  var deliveryMethod = ""
  var paymentMethod = ""

  private def checkoutTimer: Cancellable =
    system.scheduler.scheduleOnce(checkoutTimerDuration, self, ExpireCheckout)

  private def paymentTimer: Cancellable =
    system.scheduler.scheduleOnce(paymentTimerDuration, self, ExpirePayment)

  def receive: Receive = startCheckout

  def startCheckout: Receive = {
    case StartCheckout =>
      context become selectingDelivery(checkoutTimer)
  }

  def selectingDelivery(timer: Cancellable): Receive = {
    case SelectDeliveryMethod(method: String) =>
      this.deliveryMethod = method
      timer.cancel()
      context become selectingPaymentMethod(checkoutTimer)

    case ExpireCheckout =>
      timer.cancel()
      println("Checkout expired")
      context become cancelled

    case CancelCheckout =>
      timer.cancel()
      context become cancelled
  }

  def selectingPaymentMethod(timer: Cancellable): Receive = {
    case SelectPayment(method: String) =>
      this.paymentMethod = method
      timer.cancel()
      context become processingPayment(paymentTimer)

    case ExpireCheckout =>
      timer.cancel()
      println("Checkout expired")
      context become cancelled

    case CancelCheckout =>
      timer.cancel()
      context become cancelled
  }

  def processingPayment(timer: Cancellable): Receive = {
    case ConfirmPaymentReceived =>
      timer.cancel()
      context become closed

    case ExpirePayment =>
      timer.cancel()
      println("Payment expired")
      context become cancelled

    case CancelCheckout =>
      timer.cancel()
      context become cancelled
  }

  def cancelled: Receive = {
    _ => context.stop(self)
  }

  def closed: Receive = {
    _ =>
      println("Checkout closed")
      context.stop(self)
  }
}
