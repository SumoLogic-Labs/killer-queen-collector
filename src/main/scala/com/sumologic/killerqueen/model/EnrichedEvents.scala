package com.sumologic.killerqueen.model

import com.fasterxml.jackson.annotation.JsonProperty
import com.sumologic.killerqueen.model.InboundEvents._
import com.sumologic.killerqueen.state.PlayerState

/**
  * An [[EnrichedEvent]] provides additional context, such as the current state of the player executing the event.  The
  * extra metadata provided varies from each type of event.
  */
sealed trait EnrichedEvent extends Event {
  val originalEvent: InboundEvent

  val enriched = true

  val event = s"enriched_${originalEvent.event}"
}

object EnrichedEvents {


  /**
    * Generically enhance any event that is related to a single player
    */
  case class PlayerStateEnrichedEvent(currentState: PlayerState.CurrentState,
                                      originalEvent: GameplayEvent
                                     ) extends EnrichedEvent

  case class EnrichedPlayerKillEvent(@JsonProperty("player1State") murdererState: PlayerState.CurrentState,
                                     @JsonProperty("player2State") victimState: PlayerState.CurrentState,
                                     deathLocation: String,
                                     originalEvent: PlayerKillEvent
                                    ) extends EnrichedEvent

  case class EnrichedGlanceEvent(player1State: PlayerState.CurrentState,
                                 player2State: PlayerState.CurrentState,
                                 originalEvent: GlanceEvent
                                ) extends EnrichedEvent

  case class EnrichedSnailEatEvent(@JsonProperty("player1State") murdererState: PlayerState.CurrentState,
                                   @JsonProperty("player2State") victimState: PlayerState.CurrentState,
                                   originalEvent: SnailEatEvent
                                  ) extends EnrichedEvent

}
