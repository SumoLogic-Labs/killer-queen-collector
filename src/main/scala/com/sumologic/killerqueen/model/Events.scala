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

  /**
   * Occurs right as the game ends.  It always occurs just before the [[VictoryEvent]].  It mirrors the
   * [[GameStartEvent]] in terms of fields available.
   *
   * @param map      `map_day`, `map_dusk`, or `map_night`.  Will be one of these even if it's a bonus game.
   * @param unknown1 Unknown boolean argument.  Might be `attract mode` configuration.
   * @param duration Duration of the games in seconds.  Includes partial seconds (because it's a Double.)
   * @param unknown2 Unknown boolean argument.  Might be `attract mode` configuration.
   */
  case class GameEndEvent(map: String,
                          unknown1: Boolean,
                          duration: Double,
                          unknown2: Boolean)
    extends GameplayEvent("gameend", s"$map,${unknown1.toString.capitalize},$duration,${unknown2.toString.capitalize}")


  /**
   * Occurs after the 3... 2... 1... countdown.  Signals that players can start interacting with each other.  Most of
   * the fields are not useful.
   *
   * @param map                  `map_day`, `map_dusk`, or `map_night`.  Will be one of these even if it's a bonus game.
   * @param goldOnLeft           Is Gold on the left cabinet/spawn?  Default is False
   * @param duration             Always 0.0
   * @param isAttractModeEnabled Attract mode (flashing lights, etc.) is enabled
   */
  case class GameStartEvent(map: String,
                            goldOnLeft: Boolean,
                            duration: Int,
                            isAttractModeEnabled: Boolean)
    extends GameplayEvent("gamestart", s"$map,${goldOnLeft.toString.capitalize},$duration,${isAttractModeEnabled.toString.capitalize}")

  /**
   * Occurs after the [[GameEndEvent]] event.  Tells us who won and how they won.
   *
   * @param team   `Gold` or `Blue`.
   * @param `type` `military`, `economy`, or `snail`
   */
  case class VictoryEvent(team: String,
                          `type`: String)
    extends GameplayEvent("victory", s"$team," + `type`)

  /**
   * Berry deposited into hole by minion of that same team.
   *
   * @param x
   * @param y
   * @param player
   */
  case class BerryDepositEvent(x: Int,
                               y: Int,
                               player: Player)
    extends GameplayEvent("berryDeposit", s"$x,$y,${player.id}")

  /**
   * Berry kicked in by any player into any team's economy
   *
   * @param x
   * @param y
   * @param player
   * @param ownTeam Not defined on older builds.
   */
  case class BerryKickInEvent(x: Int,
                              y: Int,
                              player: Player,
                              ownTeam: Option[Boolean])
    extends GameplayEvent("berryKickIn", s"$x,$y,${player.id}${ownTeam.map(_.toString).getOrElse("").capitalize}")

  /**
   * Player picked up a piece of food.
   *
   * @param player
   */
  case class CarryFoodEvent(player: Player)
    extends GameplayEvent("carryFood", player.id.toString)

  /**
   * Queen tapped a maiden/gate
   *
   * @param x
   * @param y
   * @param team
   */
  case class BlessMaidenEvent(x: Int,
                              y: Int,
                              team: String)
    extends GameplayEvent("blessMaiden", s"$x,$y,${if (team == "Gold") "Red" else team}")

  /**
   * Minion stepped into a maiden/gate.  The minion may still choose to leave the gate as they're not being using it yet.
   *
   * @param x
   * @param y
   * @param player
   */
  case class ReserveMaidenEvent(x: Int,
                                y: Int,
                                player: Player)
    extends GameplayEvent("reserveMaiden", s"$x,$y,${player.id}")

  /**
   * Minion chose to leave the maiden/gate instead of using it.
   *
   * @param x
   * @param y
   * @param player
   */
  case class UnreserveMaidenEvent(x: Int,
                                  y: Int,
                                  player: Player)
    extends GameplayEvent("unreserveMaiden", s"$x,$y,,${player.id}")

  /**
   * Minion actually used the maiden/gate to upgrade their character.
   *
   * @param x
   * @param y
   * @param `type` - `maiden_wings` or `maiden_speed`
   * @param player
   */
  case class UseMaidenEvent(x: Int,
                            y: Int,
                            `type`: String,
                            player: Player)
    extends GameplayEvent("useMaiden", s"$x,$y," + `type` + s",${player.id}")

  /**
   * Player has jumped off the snail OR was killed on the snail.  A [[PlayerKillEvent]] with the same `x` and `y`
   * coordinates will immediately follow if they were killed.
   *
   * @param x
   * @param y
   * @param player
   */
  case class GetOffSnailEvent(x: Int,
                              y: Int,
                              player: Player)
    extends GameplayEvent("getOffSnail", s"$x,$y,,${player.id}") {
    override def toApi: String = buildApiString("getOffSnail: ", rawValue)
  }

  /**
   * Player hopped on a snail
   *
   * @param x
   * @param y
   * @param player
   */
  case class GetOnSnailEvent(x: Int,
                             y: Int,
                             player: Player)
    extends GameplayEvent("getOnSnail", s"$x,$y,${player.id}") {

    override def toApi: String = buildApiString("getOnSnail: ", rawValue)
  }

  /**
   * Snail started eating another player.  The `victim` can still be saved though.
   *
   * @param x
   * @param y
   * @param rider
   * @param victim
   */
  case class SnailEatEvent(x: Int,
                           y: Int,
                           @JsonProperty("player1") rider: Player,
                           @JsonProperty("player2") victim: Player)
    extends GameplayEvent("snailEat", s"$x,$y,${rider.id},${victim.id}")

  /**
   * The rider of the snail was killed, and thus `player` escaped being eaten
   *
   * @param x
   * @param y
   * @param player
   */
  case class SnailEscapeEvent(x: Int,
                              y: Int,
                              player: Player)
    extends GameplayEvent("snailEscape", s"$x,$y,${player.id}")

  /**
   * `player` glanced (ran into) `player2`.  (In the event of simultaneous/equal force, it appears to be randomly selected.)
   *
   * @param x Not defined on older builds.
   * @param y Not defined on older builds.
   * @param player1
   * @param player2
   */
  case class GlanceEvent(x: Option[Int],
                         y: Option[Int],
                         player1: Player,
                         player2: Player)
    extends GameplayEvent("glance", s"${x.getOrElse(0)},${y.getOrElse(0)},${player1.id},${player2.id}") {

    override def toApi: String = {
      if (x.isEmpty || y.isEmpty) {
        buildApiString("glance", s"${player1.id},${player2.id}")
      } else {
        buildApiString("glance", rawValue)
      }
    }
  }

  // Worker, Soldier, and Queen are valid victimType
  /**
   * `player1` / `killer` killed `player2` / `victim`
   *
   * @param x
   * @param y
   * @param killer
   * @param victim
   * @param victimType - `Worker`, `Soldier`, or `Queen`
   */
  case class PlayerKillEvent(x: Int,
                             y: Int,
                             @JsonProperty("player1") killer: Player,
                             @JsonProperty("player2") victim: Player,
                             victimType: String)
    extends GameplayEvent("playerKill", s"$x,$y,${killer.id},${victim.id},$victimType")

  /**
   * Unused event still sent through by the cabinet
   */
  case object PlayerNamesEvent
    extends GameplayEvent("playernames", ",,,,,,,,,")

  /**
   * `player` has joined the game.  If a bot is being replaced, this will trigger twice in a game for `player`.
   *
   * @param player
   * @param isBot
   */
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
