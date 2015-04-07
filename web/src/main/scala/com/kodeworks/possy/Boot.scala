package com.kodeworks.possy

import java.net.URLDecoder
import akka.http.model.japi.ResponseEntity
import akka.util.{ByteString, Timeout}
import com.kodeworks.possy.PossyActor.ElevationModel

import concurrent.duration._

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.http.Http
import akka.http.model.headers.HttpOriginRange.*
import akka.http.model.{ContentTypes, HttpEntity, HttpResponse, headers}
import akka.stream.ActorFlowMaterializer
import akka.stream.scaladsl.{Sink, Source}
import org.slf4j.LoggerFactory

import scala.concurrent.Future

object Boot extends App {
  val log = LoggerFactory.getLogger(Boot.getClass)
  val responseHeaders = headers.`Access-Control-Allow-Origin`(*) :: Nil

  implicit val system = ActorSystem()
  implicit val materializer = ActorFlowMaterializer()
  implicit val ec = system.dispatcher
  implicit val timeout = Timeout(5 seconds)

  val possy = system.actorOf(Props(new PossyActor()))

  val serverSource: Source[Http.IncomingConnection, Future[Http.ServerBinding]] =
    Http(system).bind(interface = "localhost", port = 8080)
  val bindingFuture: Future[Http.ServerBinding] = serverSource.to(Sink.foreach { connection =>
    println("Accepted new connection from " + connection.remoteAddress)
    connection.handleWithAsyncHandler {
      case req => {
        val data = URLDecoder.decode(req.entity.asInstanceOf[HttpEntity.Strict].data.utf8String, "UTF-8")
        import argonaut._, Argonaut._
        val p = data.decodeOption[PossyActor.LoadClosestElevationModel].get
        log.debug("parsed: {}", p)
        (possy ? p) map { r => {
          HttpResponse(headers = responseHeaders,
            entity = HttpEntity.Strict(ContentTypes.`application/json`,
              ByteString(r.asInstanceOf[PossyActor.ElevationModel].asJson.toString)))
        }
        }
      }
    }
  }).run()
}
