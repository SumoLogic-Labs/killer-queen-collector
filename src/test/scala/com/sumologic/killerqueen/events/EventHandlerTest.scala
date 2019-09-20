package com.sumologic.killerqueen.events

import com.sumologic.killerqueen.TestBase
import com.sumologic.killerqueen.model.InboundEvents._
import com.sumologic.killerqueen.model.OutboundEvents._
import com.sumologic.killerqueen.model.Player
import com.sumologic.killerqueen.state.StateMachine
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.mockito.MockitoSugar

class EventHandlerTest extends TestBase with MockitoSugar with BeforeAndAfterEach {

  "EventHandler" should {
    "always ping back for AliveEvent" in {
      sut.handle(AliveEvent("a"))
      verify(eventSender, times(1)).send(ImAliveEvent("null"))
    }

    "pass along gameplay events" in {
      val event = GlanceEvent(None, None, Player(1), Player(2))
      sut.handle(event)
      verify(stateMachine, times(1)).processEvent(event)
    }

    "not freak out on unknown events" in {
      sut.handle(UnknownEvent("!", "2"))
    }
  }

  private var eventSender: EventSender = _
  private var stateMachine: StateMachine = _
  private var sut: EventHandler = _

  override protected def beforeEach(): Unit = {
    super.beforeEach()

    eventSender = mock[EventSender]
    stateMachine = mock[StateMachine]
    sut = new EventHandler(eventSender, stateMachine)
  }

}
