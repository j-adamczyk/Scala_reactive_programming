package EShop.lab6

import java.net.URI

import EShop.lab6.ProductCatalog.{GetItems, Item, Items}
import EShop.lab6.ProductCatalogHttpServer.Response
import akka.actor.ActorSystem

import scala.concurrent.duration._
import akka.pattern.ask
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.{HttpApp, Route}
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import spray.json.{DefaultJsonProtocol, JsString, JsValue, JsonFormat}

import scala.concurrent.{Await, Future}

object ProductCatalogHttpServer {
  case class Query(brand: String, productKeyWords: List[String])
  case class Response(products: List[Item])
}

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  //custom formatter just for example
  implicit val uriFormat = new JsonFormat[java.net.URI] {
    override def write(obj: java.net.URI): spray.json.JsValue = JsString(obj.toString)
    override def read(json: JsValue): URI = json match {
      case JsString(url) => new URI(url)
      case _             => throw new RuntimeException("Parsing exception")
    }
  }

  implicit val itemFormat = jsonFormat5(Item)
  implicit val queryFormat = jsonFormat2(ProductCatalogHttpServer.Query)
  implicit val responseFormat = jsonFormat1(ProductCatalogHttpServer.Response)
}

object ProductCatalogHttpServerApp extends App {
  new ProductCatalogHttpServer().startServer("localhost", 9000)
}

/** Just to demonstrate how one can build akka-http based server with JsonSupport */
class ProductCatalogHttpServer extends HttpApp with JsonSupport {
  implicit val timeout = Timeout(5 seconds)
  val config = ConfigFactory.load()

  val actorSystem = ActorSystem("server", config.getConfig("server").withFallback(config))
  val productCatalog =
    actorSystem.actorSelection("akka.tcp://ProductCatalog@127.0.0.1:2553/user/productcatalog")

  override protected def routes: Route = {
    path("search") {
      post {
        entity(as[ProductCatalogHttpServer.Query]) { query =>
          val future = productCatalog ? GetItems(query.brand, query.productKeyWords)
          val result = Await.result(future, timeout.duration).asInstanceOf[Items]

          complete {
            Future.successful(Response(result.items))
          }
        }
      }
    }
  }
}
