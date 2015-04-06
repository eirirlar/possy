package com.kodeworks.possy

import akka.actor.{Props, ActorSystem}

object Boot extends App {
  val actorSystem = ActorSystem()
  val httpServer = actorSystem.actorOf(Props(new HttpServer))
}
