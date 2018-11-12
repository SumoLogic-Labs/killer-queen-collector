package com.sumologic.killerqueen.state

import com.sumologic.killerqueen.model.{Event, Player}

/**
  * [[PlayerState]] tracks all information related to a current [[Player]].  It is primarily focused on stats collection
  *
  * NOTE: Often there are more than 10 player states at the end of the game.  Bots are considered distinct from humans.
  *
  * @param player
  * @param isBot
  */
class PlayerState(player: Player, isBot: Boolean) {
  val id: Int = player.id
  val isQueen: Boolean = player.isQueen
  val team: String = player.team
  val name: String = player.name

  val currentState = new PlayerState.CurrentState(player, isBot)

  var foodDeposited: Int = 0
  var foodKickedInForMyTeam: Int = 0
  var foodKickedInForOtherTeam: Int = 0

  var totalLives: Int = 1
  var totalDeaths: Int = 0
  var totalKills: Int = 0

  var snailEscapes: Int = 0

  var distanceTraveledOnSnail: Int = 0

  def toFinalPlayerState: FinalPlayerState = {
    FinalPlayerState(
      id,
      player.name,
      currentState.userName,
      player.team,
      foodDeposited,
      foodKickedInForMyTeam,
      foodKickedInForOtherTeam,
      totalLives,
      totalDeaths,
      totalKills,
      snailEscapes,
      distanceTraveledOnSnail,
      isBot,
      currentState.isWarrior,
      currentState.isFast
    )
  }

  override def toString(): String = {
    toFinalPlayerState.toString
  }
}

object PlayerState {

  class CurrentState(player: Player, val isBot: Boolean) {
    var isOnSnail: Boolean = false

    var isFast: Boolean = false
    var isWarrior: Boolean = false

    var hasFood: Boolean = false

    var userName: String = if (isBot) s"Bot ${player.id}" else s"Player ${player.id}"

    def copy(): CurrentState = {
      val newState = new CurrentState(player, isBot)

      newState.isOnSnail = isOnSnail
      newState.isFast = isFast
      newState.isWarrior = isWarrior
      newState.hasFood = hasFood
      newState.userName = userName

      newState
    }
  }

}


/**
  * Built from [[PlayerState]], this is used to create the JSON event
  *
  * @param id            1-10 - from [[Player]]
  * @param name          name of position - from [[Player]]
  * @param userName      Configured name for player (if UI is used.)  "Bot $id" or "Player $id" if not set
  * @param team          `Gold` or `Blue`
  * @param foodDeposited Number of berries manually scored (always 0 for a queen)
  * @param foodKickedInForMyTeam
  * @param foodKickedInForOtherTeam
  * @param lives         Number of lives used during game.  (Usually deaths + 1, except for military victories for queens)
  * @param deaths
  * @param kills
  * @param snailEscapes  Escapes from being eaten by snail
  * @param movedSnailDistance
  * @param botAtEndOfGame
  * @param warriorAtEndOfGame
  * @param fastAtEndOfGame
  */
case class FinalPlayerState(id: Int,

                            name: String,
                            userName: String,
                            team: String,

                            foodDeposited: Int,
                            foodKickedInForMyTeam: Int,
                            foodKickedInForOtherTeam: Int,

                            lives: Int,
                            deaths: Int,
                            kills: Int,
                            snailEscapes: Int,

                            movedSnailDistance: Int,

                            botAtEndOfGame: Boolean,
                            warriorAtEndOfGame: Boolean,
                            fastAtEndOfGame: Boolean
                           ) extends Event {
  val event = "finalPlayerState"
}
