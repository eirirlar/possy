package com.kodeworks.possy

import akka.actor.{Props, ActorSystem}
import akka.http.Http
import akka.http.model.{HttpHeader, HttpResponse}
import akka.stream.ActorFlowMaterializer
import akka.stream.scaladsl.{Sink, Source}

import scala.concurrent.Future

object Boot extends App {
  implicit val system = ActorSystem()
  implicit val materializer = ActorFlowMaterializer()
  implicit val ec = system.dispatcher
  val serverSource: Source[Http.IncomingConnection, Future[Http.ServerBinding]] =
    Http(system).bind(interface = "localhost", port = 8080)
  val bindingFuture: Future[Http.ServerBinding] = serverSource.to(Sink.foreach { connection =>
    println("Accepted new connection from " + connection.remoteAddress)
    connection.handleWithAsyncHandler {
      case x => Future(HttpResponse())
    }
  }).run()
}
