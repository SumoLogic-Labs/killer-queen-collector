package com.sumologic.killerqueen.model

import com.fasterxml.jackson.annotation.JsonProperty

/**
  * An [[Event]] is what we pass around to trigger changes in things in this code base.
  */
trait Event {
  val event: String
}

/**
  * A [[WireEvent]] is something designed to be sent or receiver on the cabinet communication path
  */
sealed trait WireEvent extends Event {
  val rawValue: String

  protected def buildApiString(event: String, rawValue: String): String = s"![k[$event],v[$rawValue]]!"

  def toApi: String = buildApiString(event, rawValue)
}

/**
  * An [[InboundEvent]] is one we receive from the game cabinet over the wire
  */
sealed class InboundEvent(override val event: String, override val rawValue: String) extends WireEvent

/**
  * An [[OutboundEvent]] is one we send over the wire to the cabinet
  */
sealed class OutboundEvent(val event: String, val rawValue: String) extends WireEvent

/**
  * A [[GameplayEvent]] can trigger changes in the [[com.sumologic.killerqueen.state.StateMachine]] we're running
  */
sealed class GameplayEvent(e: String, r: String) extends InboundEvent(e, r)

/**
  * [[ResetEvent]] can be used to reset the [[com.sumologic.killerqueen.state.StateMachine]]
  */
case object ResetEvent extends Event {
  val event = "reset"
}

/**
  * Collection of [[InboundEvent]] we can receive.  Some are also [[GameplayEvent]]
  */
object InboundEvents {

  case class AliveEvent(timestamp: String)
    extends InboundEvent("alive", timestamp)

  case class ConnectedEvent(connectionId: Int)
    extends InboundEvent("connected", connectionId.toString)

  case class GameEndEvent(map: String,
                          unknown1: Boolean,
                          duration: Double,
                          unknown2: Boolean)
    extends GameplayEvent("gameend", s"$map,${unknown1.toString.capitalize},$duration,${unknown2.toString.capitalize}")

  case class GameStartEvent(map: String,
                            unknown1: Boolean,
                            duration: Int,
                            unknown2: Boolean)
    extends GameplayEvent("gamestart", s"$map,${unknown1.toString.capitalize},$duration,${unknown2.toString.capitalize}")

  case class VictoryEvent(team: String,
                          `type`: String)
    extends GameplayEvent("victory", s"$team," + `type`)

  case class BerryDepositEvent(x: Int,
                               y: Int,
                               player: Player)
    extends GameplayEvent("berryDeposit", s"$x,$y,${player.id}")

  case class BerryKickInEvent(x: Int,
                              y: Int,
                              player: Player)
    extends GameplayEvent("berryKickIn", s"$x,$y,${player.id}")

  case class CarryFoodEvent(player: Player)
    extends GameplayEvent("carryFood", player.id.toString)

  case class BlessMaidenEvent(x: Int,
                              y: Int,
                              team: String)
    extends GameplayEvent("blessMaiden", s"$x,$y,${if (team == "Gold") "Red" else team}")

  case class ReserveMaidenEvent(x: Int,
                                y: Int,
                                player: Player)
    extends GameplayEvent("reserveMaiden", s"$x,$y,${player.id}")

  case class UnreserveMaidenEvent(x: Int,
                                  y: Int,
                                  player: Player)
    extends GameplayEvent("unreserveMaiden", s"$x,$y,,${player.id}")

  case class UseMaidenEvent(x: Int,
                            y: Int,
                            `type`: String,
                            player: Player)
    extends GameplayEvent("useMaiden", s"$x,$y," + `type` + s",${player.id}")

  case class GetOffSnailEvent(x: Int,
                              y: Int,
                              player: Player)
    extends GameplayEvent("getOffSnail", s"$x,$y,,${player.id}") {
    override def toApi: String = buildApiString("getOffSnail: ", rawValue)
  }

  case class GetOnSnailEvent(x: Int,
                             y: Int,
                             player: Player)
    extends GameplayEvent("getOnSnail", s"$x,$y,${player.id}") {

    override def toApi: String = buildApiString("getOnSnail: ", rawValue)
  }

  case class SnailEatEvent(x: Int,
                           y: Int,
                           @JsonProperty("player1") rider: Player,
                           @JsonProperty("player2") victim: Player)
    extends GameplayEvent("snailEat", s"$x,$y,${rider.id},${victim.id}")

  case class SnailEscapeEvent(x: Int,
                              y: Int,
                              player: Player)
    extends GameplayEvent("snailEscape", s"$x,$y,${player.id}")

  case class GlanceEvent(player1: Player,
                         player2: Player)
    extends GameplayEvent("glance", s"${player1.id},${player2.id}")

  // Worker, Soldier, and Queen are valid victimType
  case class PlayerKillEvent(x: Int,
                             y: Int,
                             @JsonProperty("player1") killer: Player,
                             @JsonProperty("player2") victim: Player,
                             victimType: String)
    extends GameplayEvent("playerKill", s"$x,$y,${killer.id},${victim.id},$victimType")

  case object PlayerNamesEvent
    extends GameplayEvent("playernames", ",,,,,,,,,")

  case class SpawnEvent(player: Player,
                        isBot: Boolean)
    extends GameplayEvent("spawn", s"${player.id},${isBot.toString.capitalize}")

  case class UnknownEvent(override val event: String,
                          override val rawValue: String)
    extends InboundEvent(event, rawValue)

  case class UserNameUpdateEvent(blueStripes: String,
                                 blueAbs: String,
                                 blueQueen: String,
                                 blueSkulls: String,
                                 blueCheckers: String,
                                 goldStripes: String,
                                 goldAbs: String,
                                 goldQueen: String,
                                 goldSkulls: String,
                                 goldCheckers: String
                                )
    extends GameplayEvent("userNameUpdate", s"$blueStripes,$blueAbs,$blueQueen,$blueSkulls,$blueCheckers,$goldStripes,$goldAbs,$goldQueen,$goldSkulls,$goldCheckers") {


    def get(playerId: Int): Option[String] = {
      val candidate = playerId match {
        case 1 => goldQueen
        case 3 => goldStripes
        case 5 => goldAbs
        case 7 => goldSkulls
        case 9 => goldCheckers

        case 2 => blueQueen
        case 4 => blueStripes
        case 6 => blueAbs
        case 8 => blueSkulls
        case 10 => blueCheckers
      }

      Option(candidate).filter(_.trim.nonEmpty)
    }

  }

}

/**
  * Collection of [[OutboundEvent]] we may send.  These are never [[GameplayEvent]]
  */
object OutboundEvents {

  case object ImAliveEvent
    extends OutboundEvent("im alive", "null")

  case class ConnectEvent(name: String,
                          isGameMachine: Boolean)
    extends OutboundEvent("connect", s"""{"name":"$name","isGameMachine":$isGameMachine}""")

}
