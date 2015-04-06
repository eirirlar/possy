package com.kodeworks.possy

import akka.actor.Actor
import akka.http.Http
import akka.stream.ActorFlowMaterializer
import akka.stream.scaladsl.{Sink, Source}

import scala.concurrent.Future

class HttpServer extends Actor {
  implicit val system = context.system
  implicit val materializer = ActorFlowMaterializer()
  val serverSource: Source[Http.IncomingConnection, Future[Http.ServerBinding]] =
    Http(system).bind(interface = "localhost", port = 8080)
  val bindingFuture: Future[Http.ServerBinding] = serverSource.to(Sink.foreach { connection =>
    // foreach materializes the source
    println("Accepted new connection from " + connection.remoteAddress)
    // ... and then actually handle the connection
  }).run()

  override def receive = {
    case x => println(x)
  }
}
