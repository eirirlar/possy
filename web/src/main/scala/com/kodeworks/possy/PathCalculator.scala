package com.kodeworks.possy

import akka.actor.Actor
import org.slf4j.LoggerFactory
import PathCalculator._

class PathCalculator extends Actor {
  override def receive = {
    case m => {
      log.debug("pathCalculator message")
    }
  }
}

object PathCalculator {
  val log = LoggerFactory.getLogger(classOf[PathCalculator])
}
