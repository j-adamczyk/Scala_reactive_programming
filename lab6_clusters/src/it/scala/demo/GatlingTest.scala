package demo

import io.gatling.core.Predef.{Simulation, StringBody, jsonFile, rampUsers, scenario, _}
import io.gatling.http.Predef.http

import scala.concurrent.duration._

class HttpWorkerGatlingTest extends Simulation {

  val httpProtocol = http
    .baseUrls("http://localhost:9000")
    .acceptHeader("text/plain,text/html,application/json,application/xml;")
    .userAgentHeader("Mozilla/5.0 (Windows NT 5.1; rv:31.0) Gecko/20100101 Firefox/31.0")

  val scn = scenario("BasicSimulation")
    .feed(jsonFile(classOf[HttpWorkerGatlingTest].getResource("/data/search.json").getPath).random)
    .exec(
      http("search_basic")
        .post("/search")
        .body(StringBody("""{ "brand": "${brand}", "productKeywords": ["${productKeywords}"]}"""))
        .asJson
    )
    .pause(5)

  setUp(
    scn.inject(rampUsers(7).during(1.minutes))
  ).protocols(httpProtocol)
}