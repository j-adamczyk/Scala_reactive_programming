package EShop.lab2

import EShop.lab2
import akka.actor.Cancellable
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}

import scala.language.postfixOps
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import EShop.lab3.{OrderManager, TypedPayment}

object TypedCheckout {

  sealed trait Data
  case object Uninitialized                               extends Data
  case class SelectingDeliveryStarted(timer: Cancellable) extends Data
  case class ProcessingPaymentStarted(timer: Cancellable) extends Data

  sealed trait Command
  case object StartCheckout                                                                       extends Command
  case class SelectDeliveryMethod(method: String)                                                 extends Command
  case object CancelCheckout                                                                      extends Command
  case object ExpireCheckout                                                                      extends Command
  case class SelectPayment(payment: String, orderManagerRef: ActorRef[OrderManager.Command]) extends Command
  case object ExpirePayment                                                                       extends Command
  case object ConfirmPaymentReceived                                                              extends Command

  sealed trait Event
  case object CheckOutClosed                           extends Event
  case class PaymentStarted(paymentRef: ActorRef[Any]) extends Event
}


class TypedCheckout(
  cartActor: ActorRef[TypedCartActor.Command]
) {
  import TypedCheckout._

  val checkoutTimerDuration: FiniteDuration = 1 seconds
  val paymentTimerDuration: FiniteDuration  = 1 seconds

  private def scheduleCheckoutTimer(context: ActorContext[TypedCheckout.Command]): Cancellable = {
    context.scheduleOnce(checkoutTimerDuration, context.self, ExpireCheckout)
  }

  private def schedulePaymentTimer(context: ActorContext[TypedCheckout.Command]): Cancellable = {
    context.scheduleOnce(paymentTimerDuration, context.self, ExpirePayment)
  }

  def start: Behavior[TypedCheckout.Command] = Behaviors.receive(
    (context, msg) =>
      msg match {
        case StartCheckout =>
          selectingDelivery(scheduleCheckoutTimer(context))
      }
  )

  def selectingDelivery(timer: Cancellable): Behavior[TypedCheckout.Command] = Behaviors.receive(
    (context, msg) =>
      msg match {
        case SelectDeliveryMethod(_) =>
          timer.cancel()
          selectingPaymentMethod(scheduleCheckoutTimer(context))

        case ExpireCheckout | CancelCheckout =>
          timer.cancel()
          cancelled

      }
  )

  def selectingPaymentMethod(timer: Cancellable): Behavior[TypedCheckout.Command] = Behaviors.receive(
    (context, msg) =>
      msg match {
        case SelectPayment(method: String, orderManagerRef: ActorRef[OrderManager.Command]) =>
          timer.cancel()
          val child = context.spawn(new TypedPayment(method, orderManagerRef, context.self).start, "payment")
          orderManagerRef ! OrderManager.ConfirmPaymentStarted(child)
          processingPayment(schedulePaymentTimer(context))

        case ExpireCheckout | CancelCheckout =>
          timer.cancel()
          cancelled

      }
  )

  def processingPayment(timer: Cancellable): Behavior[TypedCheckout.Command] = Behaviors.receive(
    (context, msg) =>
      msg match {
        case ConfirmPaymentReceived =>
          timer.cancel()
          closed

        case ExpirePayment | CancelCheckout =>
          timer.cancel()
          cancelled
        }
  )

  def cancelled: Behavior[TypedCheckout.Command] = Behaviors.receive(
    (context, msg) =>
      msg match {
        case _ => Behaviors.same
      }
  )

  def closed: Behavior[TypedCheckout.Command] = Behaviors.receive(
    (context, msg) =>
      msg match {
        case _ => Behaviors.same
      }
  )

}
