package EShop.lab2

import akka.actor.{ActorSystem, Props}

import scala.io.StdIn.readLine
import CartActor.{StartCheckout => CartActorStartCheckout, _}
import Checkout.{StartCheckout => CheckoutStartCheckout, _}

object EShopApp  extends App{
  val system = ActorSystem("EShop")
  val cartActor = system.actorOf(Props[CartActor], "mainActor")

  while (true) {
    println("Available operations:\n" +
            "1) AddItem item_name\n" +
            "2) RemoveItem item_name\n" +
            "3) Checkout")
    cartActor ! "print"
    val input = readLine()

    if (input.startsWith("AddItem")) {
      val elems = input.split(" ")
      if (elems.size != 2)
        println("Malformed command")
      else {
        val item = elems(1)
        cartActor ! AddItem(item)
      }
    }
    else if (input.startsWith("RemoveItem")) {
      val elems = input.split(" ")
      if (elems.size != 2)
        println("Malformed command")
      else {
        val item = elems(1)
        cartActor ! RemoveItem(item)
      }
    }
    else if (input == "Checkout") {
      cartActor ! CartActorStartCheckout
      val checkoutActor = system.actorOf(Props[Checkout], "checkoutActor")
      checkoutActor ! CheckoutStartCheckout
      println("Enter delivery method (or cancel):")
      val deliveryMethod = readLine()

      if (deliveryMethod.toLowerCase() == "cancel") {
        checkoutActor ! CancelCheckout
        cartActor ! ConfirmCheckoutCancelled
      }
      else {
        checkoutActor ! SelectDeliveryMethod(deliveryMethod)
        println("Enter payment method (or cancel):")
        val paymentMethod = readLine()

        if (paymentMethod.toLowerCase() == "cancel") {
          checkoutActor ! CancelCheckout
          cartActor ! ConfirmCheckoutCancelled
        }
        else {
          checkoutActor ! SelectPayment(paymentMethod)
          println("Pay (enter \"pay\") (or cancel):")
          val payment = readLine()

          if (payment.toLowerCase() == "cancel") {
            checkoutActor ! CancelCheckout
            cartActor ! ConfirmCheckoutCancelled
          }
          else {
            checkoutActor ! ConfirmPaymentReceived
            cartActor ! ConfirmCheckoutClosed
          }
        }
      }
    }
    else
      println("Command " + input + " not recognized")
  }
}
