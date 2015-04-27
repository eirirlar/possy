package com.kodeworks.possy

import java.net.URLDecoder

import akka.pattern.ask
import akka.actor.{ActorSystem, Props}
import akka.http.Http
import akka.http.model._
import akka.http.model.headers.HttpOriginRange.*
import akka.http.server.Directives._
import akka.http.server.{StandardRoute, Directives, Route}
import akka.stream.ActorFlowMaterializer
import akka.stream.scaladsl.{Sink, Source}
import akka.util.{ByteString, Timeout}
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory

import scala.concurrent.Future
import scala.concurrent.duration._
import Model._

object Boot extends App {
  val log = LoggerFactory.getLogger(Boot.getClass)
  val lookup = "akka://possy/user/"
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
    val route =
      (path("possy" / "loadClosestElevationIfChanged") & post & extractRequest)(req => {
        val data = URLDecoder.decode(req.entity.asInstanceOf[HttpEntity.Strict].data.utf8String, "UTF-8")
        import argonaut._
        import Argonaut._
        val p = data.decodeOption[LatLng].get
        log.debug("parsed: {}", p)
        complete((possy ? p) map { r => {
          HttpResponse(headers = responseHeaders,
            entity = HttpEntity.Strict(ContentTypes.`application/json`,
              ByteString(r.asInstanceOf[PossyActor.ElevationModel].asJson.toString)))
        }
        })
      }) ~
        (path("possy" / "path") & get & extractRequest)(req => {
          val pathCalculator = system.actorOf(Props(new PathCalculator))
          log.debug("creating pathCalculator with path: {}", pathCalculator.path)
          complete(HttpResponse(headers = responseHeaders, entity = HttpEntity.Strict(ContentTypes.`text/plain(UTF-8)`,
            ByteString(pathCalculator.path.name))))
        }) ~
        (path("possy" / "path" / Segment / "calcPath") & post & extractRequest)((pathId, req) => {
          val data = URLDecoder.decode(req.entity.asInstanceOf[HttpEntity.Strict].data.utf8String, "UTF-8")
          import argonaut._
          import Argonaut._
          val p = data.decodeOption[LatLng].get
          log.debug("parsed: {}", p)
          complete((system.actorSelection(lookup + pathId) ? CalcPath(p)) map { r => {
            HttpResponse(headers = responseHeaders,
              entity = HttpEntity.Strict(ContentTypes.`application/json`,
                ByteString(r.asInstanceOf[List[(Float, Float)]].asJson.toString)))
          }
          })
        }) ~
        (path("possy" / "path" / Segment / "getElevation") & post & extractRequest)((pathId, req) => {
          val data = URLDecoder.decode(req.entity.asInstanceOf[HttpEntity.Strict].data.utf8String, "UTF-8")
          import argonaut._
          import Argonaut._
          val p = data.decodeOption[LatLng].get
          log.debug("parsed: {}", p)
          complete((system.actorSelection(lookup + pathId) ? GetElevation(p)) map { r => {
            HttpResponse(headers = responseHeaders,
              entity = HttpEntity.Strict(ContentTypes.`application/json`,
                ByteString(r.asInstanceOf[Short].asJson.toString)))
          }
          })
        }) ~
        (path("possy" / "path" / Segment / "resetCalc") & get)(pathId => {
          complete((system.actorSelection(lookup + pathId) ? ResetCalc) map { r => {
            HttpResponse(headers = responseHeaders)
          }
          })
        })
    connection.handleWithAsyncHandler(Route.asyncHandler(route))
  }).run()
}
