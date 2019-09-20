package com.sumologic.killerqueen.events

import akka.actor.ActorRef
import akka.http.scaladsl.model.ws.TextMessage
import com.sumologic.killerqueen.Logging
import com.sumologic.killerqueen.model.OutboundEvent

trait EventSender {
  def send(event: OutboundEvent): Unit
}


class ActorRefEventSender(actorRef: ActorRef, rawMessageRecorder: RawMessageRecorder)
  extends EventSender with Logging {
  override def send(event: OutboundEvent): Unit = {
    debug(s"Sending $event")
    rawMessageRecorder.writeToFile(event.toApi)
    actorRef ! TextMessage.Strict(event.toApi)
  }
}

object NoopEventSender extends EventSender {
  override def send(event: OutboundEvent): Unit = {}
}
