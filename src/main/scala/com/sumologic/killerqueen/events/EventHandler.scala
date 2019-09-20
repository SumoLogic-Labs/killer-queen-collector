package com.sumologic.killerqueen.events

import com.sumologic.killerqueen.Logging
import com.sumologic.killerqueen.model.InboundEvents._
import com.sumologic.killerqueen.model.OutboundEvents._
import com.sumologic.killerqueen.model.{GameplayEvent, InboundEvent}
import com.sumologic.killerqueen.state.StateMachine

/**
 * Handles all incoming events and delegates relevant [[GameplayEvent]] to [[StateMachine]].
 *
 * @param eventSender
 * @param stateMachine Allows injection of mocked or test [[StateMachine]] for UTs
 * @param victoryHook  Callback hook used for helping with UTs.  Called before [[StateMachine]] is reset
 */
class EventHandler(eventSender: EventSender,
                   stateMachine: StateMachine = new StateMachine,
                   victoryHook: () => Unit = () => {}
                  ) extends Logging {
  def handle(event: InboundEvent): Unit = {
    event match {
      case AliveEvent(_) =>
        eventSender.send(ImAliveEvent)

      case UnknownEvent(key, value) =>
        warn(s"Encountered unknown event. key: $key - value: $value")

      case ConnectedEvent(connectionId) =>
        info(s"Connection opened to cabinet completed ($connectionId)")
        stateMachine.reset(event)
        eventSender.send(AdminLogin)

      case LoginEvent(id) =>
        info(s"Login succeeded")
        eventSender.send(GetConfigEvent("goldonleft")) // TODO: This doesn't actually work, despite suggestions that it does.  Needs further investigation.
        eventSender.send(GetConfigEvent("tournamentstatus")) // Proof that this GetConfigEvent works

      case victoryEvent: VictoryEvent =>
        stateMachine.processEvent(victoryEvent)
        victoryHook()
        stateMachine.reset(event)

      case gameplayEvent: GameplayEvent =>
        stateMachine.processEvent(gameplayEvent)

      case _ =>
        error(s"Encountered completely unknown event type: ${event.toApi} (scala: $event)")
    }
  }
}
