package com.sumologic.killerqueen.model

import com.fasterxml.jackson.annotation.JsonProperty
import com.sumologic.killerqueen.model.InboundEvents._
import com.sumologic.killerqueen.state.PlayerState

/**
  * An [[EnrichedEvent]] provides additional context, such as the current state of the player executing the event.  The
  * extra metadata provided varies from each type of event.
  */
sealed trait EnrichedEvent extends Event {
  /**
    * [[InboundEvent]] that led us to provide enriched information
    */
  val originalEvent: InboundEvent

  val enriched = true

  val event = s"enriched_${originalEvent.event}"
}

object EnrichedEvents {


  /**
    * Generically enhance any event that is related to just a single player
    *
    * @param currentState Snapshot of the player's current state.  May occur before or after applying the
    *                     `originalEvent` - whichever makes it a more interesting piece of information
    * @param originalEvent
    */
  case class PlayerStateEnrichedEvent(currentState: PlayerState.CurrentState,
                                      originalEvent: GameplayEvent
                                     ) extends EnrichedEvent

  /**
    *
    * @param murdererState
    * @param victimState   Snapshot of the victim's state just before death
    * @param deathLocation Calculated by examining the `x` and `y` coordinates of `originalEvent` and using the current map for more information
    * @param originalEvent
    */
  case class EnrichedPlayerKillEvent(@JsonProperty("player1State") murdererState: PlayerState.CurrentState,
                                     @JsonProperty("player2State") victimState: PlayerState.CurrentState,
                                     deathLocation: String,
                                     originalEvent: PlayerKillEvent
                                    ) extends EnrichedEvent

  case class EnrichedGlanceEvent(player1State: PlayerState.CurrentState,
                                 player2State: PlayerState.CurrentState,
                                 originalEvent: GlanceEvent
                                ) extends EnrichedEvent

  /**
    *
    * @param murdererState
    * @param victimState Snapshot of victim's state as they begin to be eaten
    * @param originalEvent
    */
  case class EnrichedSnailEatEvent(@JsonProperty("player1State") murdererState: PlayerState.CurrentState,
                                   @JsonProperty("player2State") victimState: PlayerState.CurrentState,
                                   originalEvent: SnailEatEvent
                                  ) extends EnrichedEvent

}
