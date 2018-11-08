package com.sumologic.killerqueen.events

import com.sumologic.killerqueen.TestBase
import com.sumologic.killerqueen.model.InboundEvents.ConnectedEvent
import com.sumologic.killerqueen.state.GameState

class EventJsonSerializerTest extends TestBase {

  "EventJsonSerializer" should {
    "convert objects to get json" in {
      val gs = new GameState
      val event = ConnectedEvent(1)
      val sut = new EventJsonSerializer(gs)
      sut.toJson(event) should be(
        s"""{"connectionId":1,"event":"${event.event}","rawValue":"${event.rawValue}","gameId":${gs.id},"apiVersion":${sut.ApiVersion},"secondsSinceGameStart":0}""")
    }
  }

}
