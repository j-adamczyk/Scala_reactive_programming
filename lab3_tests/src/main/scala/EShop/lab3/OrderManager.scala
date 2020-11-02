package EShop.lab3

import EShop.lab2
import EShop.lab2.{TypedCartActor, TypedCheckout}
import EShop.lab3.OrderManager._
import akka.actor.{Actor, ActorRef}
import akka.event.LoggingReceive
import akka.actor.typed.scaladsl.adapter._
import akka.actor._


object OrderManager {
  sealed trait Command
  case class AddItem(id: String)                                               extends Command
  case class RemoveItem(id: String)                                            extends Command
  case class SelectDeliveryAndPaymentMethod(delivery: String, payment: String) extends Command
  case object Buy                                                              extends Command
  case object Pay                                                              extends Command
  case class ConfirmCheckoutStarted(checkoutRef: akka.actor.typed.ActorRef[TypedCheckout.Command])  extends Command
  case class ConfirmPaymentStarted(paymentRef: akka.actor.typed.ActorRef[TypedPayment.Command])     extends Command
  case object ConfirmPaymentReceived                                           extends Command

  sealed trait Ack
  case object Done extends Ack //trivial ACK
}

class OrderManager extends Actor {
  override def receive: Receive = uninitialized

  def uninitialized: Receive = LoggingReceive {
    case AddItem(item) =>
      val typedCartActor: akka.actor.typed.ActorRef[lab2.TypedCartActor.Command] =
        context.spawn(new TypedCartActor().start, "typedCartActor")
      typedCartActor ! TypedCartActor.AddItem(item)
      context become open(typedCartActor)
      sender() ! Done
  }

  def open(cartActor: akka.actor.typed.ActorRef[TypedCartActor.Command]): Receive = LoggingReceive {
    case AddItem(item) =>
      cartActor ! TypedCartActor.AddItem(item)
      sender() ! Done

    case RemoveItem(item) =>
      cartActor ! TypedCartActor.RemoveItem(item)
      sender() ! Done

    case Buy =>
      cartActor ! TypedCartActor.StartCheckout(context.self)
      context become inCheckout(cartActor, sender)
  }


  def inCheckout(cartActorRef: akka.actor.typed.ActorRef[TypedCartActor.Command],
                 senderRef: ActorRef): Receive = LoggingReceive {
    case TypedCartActor.CheckoutStarted(checkoutRef) =>
      context become inCheckout(checkoutRef)
      senderRef ! Done

    case _ =>
      println("empty " + sender())
      self forward _
  }

  def inCheckout(checkoutActorRef: akka.actor.typed.ActorRef[TypedCheckout.Command]): Receive = LoggingReceive {
    case OrderManager.SelectDeliveryAndPaymentMethod(delivery, payment) =>
      checkoutActorRef ! TypedCheckout.SelectDeliveryMethod(delivery)
      checkoutActorRef ! TypedCheckout.SelectPayment(payment, context.self)
      context become inPayment(sender)

    case TypedCartActor.ConfirmCheckoutClosed =>
      context become finished
  }

  def inPayment(senderRef: ActorRef): Receive = LoggingReceive {
    case OrderManager.ConfirmPaymentStarted(paymentRef) =>
      context become inPayment(paymentRef, senderRef)
      senderRef ! Done
  }

  def inPayment(paymentActorRef: akka.actor.typed.ActorRef[TypedPayment.Command], senderRef: ActorRef): Receive = {
    case Pay =>
      paymentActorRef ! TypedPayment.DoPayment
      context become inPayment(paymentActorRef, sender)

    case OrderManager.ConfirmPaymentReceived =>
      context become finished
      senderRef ! Done
  }

  def finished: Receive = {
    case _ =>
      sender() ! "order manager finished job"
  }
}
