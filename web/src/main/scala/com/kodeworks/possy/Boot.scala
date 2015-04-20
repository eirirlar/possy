package com.kodeworks.possy

import java.net.URLDecoder

import akka.actor.{ActorSystem, Props}
import akka.http.Http
import akka.http.model.headers.HttpOriginRange.*
import akka.http.model._
import akka.pattern.ask
import akka.stream.ActorFlowMaterializer
import akka.stream.scaladsl.{Sink, Source}
import akka.util.{ByteString, Timeout}
import argonaut.JString
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory

import scala.concurrent.Future
import scala.concurrent.duration._

object Boot extends App {
  val log = LoggerFactory.getLogger(Boot.getClass)
  val responseHeaders = headers.`Access-Control-Allow-Origin`(*) :: Nil

  val config = ConfigFactory
    .systemProperties()
    .withFallback(ConfigFactory.load)

  val demPath = config.getString("dem.path")

  implicit val system = ActorSystem("possy", config)
  implicit val materializer = ActorFlowMaterializer()
  implicit val ec = system.dispatcher
  implicit val timeout = Timeout(5 seconds)

  val possy = system.actorOf(Props(new PossyActor(demPath)))

  val serverSource: Source[Http.IncomingConnection, Future[Http.ServerBinding]] =
    Http(system).bind(interface = "localhost", port = 8080)
  val bindingFuture: Future[Http.ServerBinding] = serverSource.to(Sink.foreach { connection =>
    println("Accepted new connection from " + connection.remoteAddress)
    connection.handleWithAsyncHandler {
      case req if (req.uri.path.reverse.startsWith(Uri.Path("loadClosestElevationIfChanged"))) => {
        val data = URLDecoder.decode(req.entity.asInstanceOf[HttpEntity.Strict].data.utf8String, "UTF-8")
        import argonaut._
        import Argonaut._
        val p = data.decodeOption[PossyActor.LoadClosestElevationModel].get
        log.debug("parsed: {}", p)
        (possy ? p) map { r => {
          HttpResponse(headers = responseHeaders,
            entity = HttpEntity.Strict(ContentTypes.`application/json`,
              ByteString(r.asInstanceOf[PossyActor.ElevationModel].asJson.toString)))
        }
        }
      }
      case req if (req.uri.path.reverse.startsWith(Uri.Path("pathId"))) => {
        val pathCalculator = system.actorOf(Props(new PathCalculator))
        log.debug("creating pathCalculator with id: {}", pathCalculator.path.name)
        Future(HttpResponse(headers = responseHeaders, entity = HttpEntity.Strict(ContentTypes.`text/plain(UTF-8)`,
          ByteString(pathCalculator.path.name))))
      }
    }
  }).run()
}
