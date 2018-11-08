package com.sumologic.killerqueen.events

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.annotation.JsonAppend
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.sumologic.killerqueen.model.{EnrichedEvent, Event, WireEvent}
import com.sumologic.killerqueen.state.GameState

class EventJsonSerializer(gameState: GameState) {
  private[this] val om = new ObjectMapper()
  om.registerModule(DefaultScalaModule)
  om.addMixIn(classOf[WireEvent], classOf[GameEvent])
  om.addMixIn(classOf[Event], classOf[GameEvent])

  val ApiVersion = 1

  def toJson(event: WireEvent): String = {
    om.
      writerFor(event.getClass).
      withAttribute("gameId", gameState.id).
      withAttribute("apiVersion", ApiVersion).
      withAttribute("secondsSinceGameStart", (System.currentTimeMillis() - gameState.startTime) / 1000).
      writeValueAsString(event)
  }

  def toJson(event: EnrichedEvent): String = {
    om.
      writerFor(event.getClass).
      withAttribute("gameId", gameState.id).
      withAttribute("apiVersion", ApiVersion).
      withAttribute("secondsSinceGameStart", (System.currentTimeMillis() - gameState.startTime) / 1000).
      writeValueAsString(event)
  }

  def toJson(event: Event): String = {
    event match {
      case e: WireEvent => toJson(e)
      case e: EnrichedEvent => toJson(e)
      case _ =>
        om.
          writerFor(event.getClass).
          withAttribute("gameId", gameState.id).
          withAttribute("apiVersion", ApiVersion).
          withAttribute("secondsSinceGameStart", (System.currentTimeMillis() - gameState.startTime) / 1000).
          writeValueAsString(event)
    }
  }

  def toJson(obj: Any): String = {
    obj match {
      case e: Event => toJson(e)
      case _ =>
        om.writeValueAsString(obj)
    }
  }
}


// https://stackoverflow.com/questions/14714328/jackson-how-to-add-custom-property-to-the-json-without-modifying-the-pojo
@JsonAppend(
  attrs = Array(
    new JsonAppend.Attr(value = "gameId"),
    new JsonAppend.Attr(value = "apiVersion"),
    new JsonAppend.Attr(value = "secondsSinceGameStart")
  )
)
trait GameEvent
