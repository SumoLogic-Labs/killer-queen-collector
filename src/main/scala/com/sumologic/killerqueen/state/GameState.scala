package com.sumologic.killerqueen.state

import com.sumologic.killerqueen.model.{Event, XYConstants}

import scala.collection.mutable

/**
  * State of a single game instance.  Object is replaced, not reset, when a new game starts.
  *
  * [[GameState]] contains a unique pattern: A ternary state dictated by Option[Boolean].  This is because until certain
  * events occur, we can not determine if we're in a bonus or demo game, and choose not to guess.
  */
class GameState {
  val id = System.currentTimeMillis()
  var startTime = 0L

  var map: Option[String] = None

  var inProgress = false
  private var _isBonusGame: Option[Boolean] = None
  private var _isDemoGame: Option[Boolean] = None

  def isBonusGame_=(newValue: Boolean): Unit = {
    if (_isBonusGame.isEmpty || _isBonusGame.get == newValue) {
      _isBonusGame = Some(newValue)
    } else {
      throw new RuntimeException(s"Conflicting heuristics for detecting bonus game state.  Was ${_isBonusGame.get} and was told to set to $newValue")
    }
  }

  def isBonusGame: Option[Boolean] = _isBonusGame

  def isDemoGame_=(newValue: Boolean): Unit = {
    if (_isDemoGame.isEmpty || _isDemoGame.get == newValue) {
      _isDemoGame = Some(newValue)
    } else {
      throw new RuntimeException(s"Conflicting heuristics for detecting demo game state.  Was ${_isDemoGame.get} and was told to set to $newValue")
    }
  }

  def isDemoGame: Option[Boolean] = _isDemoGame

  var victor: Option[String] = None
  var winType: Option[String] = None
  var duration: Option[Double] = None

  val playerMap: mutable.Map[Int, PlayerState] = mutable.Map.empty
  val playerList: mutable.Buffer[PlayerState] = mutable.Buffer[PlayerState]()

  var lastKnownSnailPosition: Int = XYConstants.ScreenWidth / 2


  def toFinalGameState: FinalGameState = {
    val queenLives = if (isBonusGame.contains(true)) {
      2
    } else {
      3
    }

    val goldQueenDeaths = playerMap.get(1).map(_.totalDeaths).getOrElse(0)
    val blueQueenDeaths = playerMap.get(2).map(_.totalDeaths).getOrElse(0)

    def berriesScored(team: String, players: Seq[PlayerState], otherTeam: Seq[PlayerState]): Int = {
      if (winType.contains("economic") && victor.contains(team)) {
        12
      } else {
        players.map {
          player => player.foodKickedInForMyTeam + player.foodDeposited
        }.sum + otherTeam.map(_.foodKickedInForOtherTeam).sum
      }
    }

    val (goldPlayers, bluePlayers) = playerList.partition(_.team == "Gold")
    val goldBerriesScored = berriesScored("Gold", goldPlayers, bluePlayers)
    val blueBerriesScored = berriesScored("Blue", bluePlayers, goldPlayers)

    def playerTypes(team: String): (Int, Int, Int, Int, Int) = {
      val players = playerMap.values.filter(p => !p.isQueen && p.team == team)

      var bots = 0
      var normalWorkers = 0
      var speedWorkers = 0
      var normalWarriors = 0
      var speedWarriors = 0

      players.foreach {
        player =>
          if (player.currentState.isBot) {
            bots += 1
          } else if (player.currentState.isWarrior) {
            if (player.currentState.isFast) {
              speedWarriors += 1
            } else {
              normalWarriors += 1
            }
          } else  {
            if (player.currentState.isFast) {
              speedWorkers += 1
            } else {
              normalWorkers += 1
            }
          }
      }

      (bots, normalWorkers, speedWorkers, normalWarriors, speedWarriors)
    }

    val (blueBots, blueNormalWorkers, blueSpeedWorkers, blueNormalWarriors, blueSpeedWarriors) = playerTypes("Blue")
    val (goldBots, goldNormalWorkers, goldSpeedWorkers, goldNormalWarriors, goldSpeedWarriors) = playerTypes("Gold")

    FinalGameState(
      id,
      map.getOrElse("UNKNOWN"),
      victor.getOrElse("NO VICTOR"),
      winType.getOrElse("NO WIN TYPE"),
      duration.getOrElse(Double.MinValue),
      isBonusGame.getOrElse(false),

      queenLives - goldQueenDeaths,
      queenLives - blueQueenDeaths,

      12 - goldBerriesScored,
      12 - blueBerriesScored,

      blueBots,
      blueNormalWorkers,
      blueSpeedWorkers,
      blueNormalWarriors,
      blueSpeedWarriors,

      goldBots,
      goldNormalWorkers,
      goldSpeedWorkers,
      goldNormalWarriors,
      goldSpeedWarriors,

      lastKnownSnailPosition
    )
  }

  override def toString(): String = {
    toFinalGameState.toString
  }
}

/**
  * Built from [[GameState]], this is used to create the JSON event. This is overloaded to also be `gameState` event type.
  *
  * @param id                     Unique ID for the run.  Is a unix timestamp triggered when [[StateMachine.reset()]] is called
  * @param map                    `map_day`, `map_dusk`, `mao_night`, or `UNKNOWN` (error case only.)  Does not denote bonus game
  * @param victor                 `Gold` or `Blue` - winning team
  * @param winType                `military`, `economy`, `snail`, or `NO WIN TYPE` (error case only.)
  * @param duration               Duration of game in seconds ([[Double.MinValue]] if unknown)
  * @param isBonusGame
  * @param goldQueenLivesRemaining
  * @param blueQueenLivesRemaining
  * @param goldBerriesRemaining
  * @param blueBerriesRemaining
  * @param blueBots
  * @param blueNormalWorkers
  * @param blueSpeedWorkers
  * @param blueNormalWarriors
  * @param blueSpeedWarriors
  * @param goldBots
  * @param goldNormalWorkers
  * @param goldSpeedWorkers
  * @param goldNormalWarriors
  * @param goldSpeedWarriors
  * @param lastKnownSnailPosition x coordinate of last known position of snail (can be potentially inaccurate)
  */
case class FinalGameState(id: Long,
                          map: String,
                          victor: String,
                          winType: String,
                          duration: Double,
                          isBonusGame: Boolean,

                          goldQueenLivesRemaining: Int,
                          blueQueenLivesRemaining: Int,

                          goldBerriesRemaining: Int,
                          blueBerriesRemaining: Int,

                          blueBots: Int,
                          blueNormalWorkers: Int,
                          blueSpeedWorkers: Int,
                          blueNormalWarriors: Int,
                          blueSpeedWarriors: Int,

                          goldBots: Int,
                          goldNormalWorkers: Int,
                          goldSpeedWorkers: Int,
                          goldNormalWarriors: Int,
                          goldSpeedWarriors: Int,

                          lastKnownSnailPosition: Int) extends Event {
  val event = "finalGameState" // This is also gameState.
}
