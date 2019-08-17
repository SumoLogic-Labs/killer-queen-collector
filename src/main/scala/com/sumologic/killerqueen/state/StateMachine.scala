package com.sumologic.killerqueen.state

import com.sumologic.killerqueen.Logging
import com.sumologic.killerqueen.events.EventJsonSerializer
import com.sumologic.killerqueen.model.EnrichedEvents._
import com.sumologic.killerqueen.model.InboundEvents._
import com.sumologic.killerqueen.model._

import scala.collection.mutable

/**
 * A [[StateMachine]] processes all [[GameplayEvent]]s and tracks the current [[GameState]] and [[PlayerState]] for all
 * players.  It includes logic to detect "demo games", which are the games being shown when the KQ machine is idle.  Also,
 * using other heuristics, it can detect if we're playing a bonus game.  (However, until a heuristic matches, it may
 * incorrectly detect that we're in a regular game.)
 *
 * @param exitOnTest Call System.exit() on failure.  Useful to forcefully crash a UT on exception.  (May be better options to consider.)
 */
class StateMachine(exitOnTest: Boolean = false) extends Logging {

  // VisibleForTesting - used to make sure certain events are happening
  private[state] var exceptionFound = false
  private[state] var gamesPlayed = 0

  // VisibleForTesting - these five are reset on reset() calls
  private[state] var gameState: GameState = new GameState
  private[state] var lastSeenEvent: Option[GameplayEvent] = None
  private var logQueue: mutable.Buffer[Event] = mutable.Buffer[Event]()
  private var allEvents: mutable.Buffer[Event] = mutable.Buffer[Event]()
  private var eventSerializer = new EventJsonSerializer(gameState)

  // This is not reset on reset() calls
  private var currentNamesOpt: Option[UserNameUpdateEvent] = None

  reset(ResetEvent) // Force above into a reasonable state

  private[this] implicit def findInPlayerMap(player: Player): PlayerState = gameState.playerMap(player.id)

  def processEvent(event: GameplayEvent): Unit = synchronized {
    if (event.isInstanceOf[UserNameUpdateEvent]) {
      updateUserNames(event.asInstanceOf[UserNameUpdateEvent])
    } else if (lastSeenEvent.nonEmpty || event.isInstanceOf[SpawnEvent]) {
      // We want to log enriched events.  Each update call can potentially return a new one.
      var enrichedEventToLog: Option[EnrichedEvent] = None

      /**
       * Sometimes we want to log an additional [[EnrichedEvent]].  This utility method keeps track of the ones to log.
       * We have a hard constraint on only logging one [[EnrichedEvent]] per normal [[Event]].  The reasoning is that
       * only one place truly owns an [[Event]], and thus only one place should record the [[EnrichedEvent]].
       */
      def possiblyLog(newConsideration: Option[EnrichedEvent]): Unit = {
        if (newConsideration.isDefined) {
          if (enrichedEventToLog.isDefined) {
            throw new Exception(s"Attempting to log two enriched events!  eventToLog=$enrichedEventToLog and newConsideration=$newConsideration")
          } else {
            enrichedEventToLog = newConsideration
          }
        }
      }

      try {
        possiblyLog(updateGameControlState(event))
        possiblyLog(updatePlayerState(event))
        possiblyLog(updateBerryState(event))
        possiblyLog(updateSnailState(event))
        possiblyLog(updateOtherStats(event))
      } catch {
        case e: Exception =>
          error(e, s"Exception when processing $event.  Current state is ${gameState.toFinalGameState} and playerMap is ${gameState.playerMap} and events to this point are ${allEvents.mkString(", ")}")
          exceptionFound = true
          if (exitOnTest) {
            System.exit(1)
          }
      }

      lastSeenEvent = Some(event)

      logEvent(event)
      enrichedEventToLog.foreach(logEvent)
    } else {
      warn(s"Ignoring event sent before game can start: $event")
    }
  }

  def currentUsers: UserNameUpdateEvent = synchronized {
    currentNamesOpt.getOrElse {
      UserNameUpdateEvent("", "", "", "", "", "", "", "", "", "")
    }
  }

  private[this] def updateUserNames(e: UserNameUpdateEvent): Unit = {
    currentNamesOpt = Some(e)
    gameState.playerMap.foreach {
      case (id, state) =>
        e.get(id).foreach(state.currentState.userName = _)
    }
  }

  private[this] val Player1 = Player(1)

  /**
   * Game control state involves things such as game start/end, isDemoGame, and isBonusGame.
   */
  private[this] def updateGameControlState(event: GameplayEvent): Option[EnrichedEvent] = {
    def logQueuedEvents(): Unit = {
      logQueue.foreach(logEvent)
      logQueue.clear()
    }

    def markAsRegularGame(): Unit = {
      debug(s"Determined it is a regular game.  Dumping ${logQueue.size} logs now.  Triggered by $event")
      gameState.gameType = GameType.RegularGame
      logQueuedEvents()
    }

    def markAsBonusGame(): Unit = {
      debug(s"Determined it is a bonus game.  Triggered by $event")
      gameState.gameType = GameType.MilitaryBonusGame
      logQueuedEvents()
      gameState.playerList.foreach {
        playerState =>
          if (playerState.totalDeaths == 0) {
            playerState.currentState.isFast = true
            playerState.currentState.isWarrior = !playerState.isQueen
          }
      }
    }

    def markAsDemoGame(): Unit = {
      debug(s"Determined it is a demo game.  Triggered by $event")
      gameState.gameType = GameType.DemoGame
      logQueue.clear()
    }

    event match {
      case GameEndEvent(_, _, duration, _) =>
        gameState.inProgress = false
        gameState.duration = Some(duration)
        gameState.ensureNot(GameType.DemoGame) // No end event for demo games

      case GameStartEvent(map, _, _, _) =>
        gameState.map = Some(map)
        gameState.inProgress = true
        gameState.startTime = System.currentTimeMillis()
        gameState.ensureNot(GameType.DemoGame) // No start event for demo games

        if (gameState.playerList.length < 10) {
          // Bonus game can start without all ten people
          markAsBonusGame()
        }

      case VictoryEvent(team, tpe) =>
        gameState.victor = Some(team)
        gameState.winType = Some(tpe)

        gameState.ensureNot(GameType.DemoGame) // No victory event for demo games

        if (tpe == "economic" && (gameState.gameType == GameType.MilitaryBonusGame || gameState.gameType == GameType.SnailBonusGame)) {
          throw new Exception("Invariant failed:  Economic victory in a bonus game")
        } else if (tpe == "military" && Player(1).totalDeaths <= 2 && Player(2).totalDeaths <= 2) {
          markAsBonusGame()
        }

        gamesPlayed += 1

      case SpawnEvent(_, true) =>
        // There are no bots in the bonus or demo games.  Bots also spawn after queens, so we know that this won't need
        // to trigger the "already spawned" code path
        markAsRegularGame()

      case SpawnEvent(Player1, false) if gameState.playerMap.contains(1) =>
        // If we're asked to spawn Player1 again, then we know its a new game (demo or otherwise.)  The issue is that
        // due to really fast button pushing, Players 3-10 could spawn before Player1.  (This is actually accurately
        // shown by demo script one.)
        //
        // We can work around this by resetting on a Player1 spawn, and then replaying all the spawns that happened
        // prior to ours.

        val eventsToReplay = allEvents.reverse.takeWhile(_.isInstanceOf[SpawnEvent]).map(_.asInstanceOf[SpawnEvent])

        reset(event)

        eventsToReplay.foreach(processEvent)

      case CarryFoodEvent(_) if !gameState.inProgress =>
        markAsDemoGame()

      case CarryFoodEvent(player) if player.totalDeaths == 0 =>
        // In a bonus game, you can't carry food unless you died
        gameState.ensureNot(GameType.MilitaryBonusGame)

      case BlessMaidenEvent(_, 20, _) =>
        // The only game mode with a maiden at this position is the bonus game
        markAsBonusGame()

      case _: ReserveMaidenEvent | _: UnreserveMaidenEvent | _: UseMaidenEvent =>
        // These are all events that don't happen in demo game
        gameState.ensureNot(GameType.DemoGame)

      case PlayerKillEvent(_, _, killer, victim, victimType) =>
        gameState.ensureNot(GameType.DemoGame) // Doesn't happen in demo game either

        // If the game reports a Soldier or Queen died, but we didn't think they're a warrior, then bonus game
        if (!victim.currentState.isWarrior && !victim.isQueen && (victimType == "Soldier" || victimType == "Queen")) {
          markAsBonusGame()
        }

        // If you're not a warrior AND not on the snail AND not queen, and killed someone, then bonus game)
        if (!killer.isQueen && !killer.currentState.isWarrior && !killer.currentState.isOnSnail) {
          markAsBonusGame()
        }

      case _: BerryDepositEvent | _: BerryKickInEvent =>
        // You can't score berries in the bonus game
        gameState.ensureNot(GameType.MilitaryBonusGame)
        gameState.ensureNot(GameType.SnailBonusGame)

      case _ =>
    }

    None
  }

  /**
   * Player state involves the primary mutations to [[PlayerState]].
   */
  private[this] def updatePlayerState(event: GameplayEvent): Option[EnrichedEvent] = {
    event match {
      case e@UseMaidenEvent(_, _, tpe, player) =>
        tpe match {
          case "maiden_speed" => player.currentState.isFast = true
          case "maiden_wings" => player.currentState.isWarrior = true
          case _ => warn(s"Unknown maiden type: $tpe")
        }

        if (player.currentState.isBot) {
          throw new Exception(s"$player is a bot when they used the maiden")
        }

        return Some(PlayerStateEnrichedEvent(player.currentState, e))

      case e@PlayerKillEvent(x, y, killer, victim, _) =>
        victim.totalDeaths += 1
        killer.totalKills += 1

        if (victim.isQueen) {
          val maxQueenLives = gameState.gameType match {
            case GameType.SnailBonusGame => 5
            case GameType.MilitaryBonusGame => 2
            case _ => 3
          }

          if (victim.totalDeaths < maxQueenLives) {
            victim.totalLives += 1
          }
        } else {
          victim.totalLives += 1
        }

        val deathLocation: String = if (gameState.gameType == GameType.MilitaryBonusGame || gameState.gameType == GameType.SnailBonusGame) {
          XYConstants.locationForXYOnMap(x, y, XYConstants.BonusGameMap)
        } else {
          XYConstants.locationForXYOnMap(x, y, gameState.map.getOrElse("unknown"))
        }

        val enrichedEvent = Some(EnrichedPlayerKillEvent(killer.currentState.copy(), victim.currentState.copy(),
          deathLocation, e))

        victim.currentState.isWarrior = false
        victim.currentState.isFast = false

        return enrichedEvent

      case SpawnEvent(player, isBot) =>
        val state = new PlayerState(player, isBot)
        gameState.playerMap.put(player.id, state)
        gameState.playerList.append(state)

        if (gameState.gameType == GameType.MilitaryBonusGame) {
          // We're ok to use ternary here.  We promote stats whenever we detect a bonus game as well
          player.currentState.isFast = true
          player.currentState.isWarrior = !player.isQueen
        }

        if (!isBot) {
          currentNamesOpt.flatMap(_.get(player.id)).foreach(state.currentState.userName = _)
        }

      case ReserveMaidenEvent(_, _, player) if player.currentState.isBot =>
        throw new Exception(s"$player is a bot when they used the maiden")

      case UnreserveMaidenEvent(_, _, player) if player.currentState.isBot =>
        throw new Exception(s"$player is a bot when they used the maiden")

      case e@ReserveMaidenEvent(_, _, player) =>
        return Some(PlayerStateEnrichedEvent(player.currentState, e))

      case e@UnreserveMaidenEvent(_, _, player) =>
        return Some(PlayerStateEnrichedEvent(player.currentState, e))

      case _ =>
    }

    None
  }

  /**
   * Tracking berries, including those scored, happens here.
   */
  private[this] def updateBerryState(event: GameplayEvent): Option[EnrichedEvent] = {
    event match {
      case e@BerryDepositEvent(x, _, player) =>
        player.currentState.hasFood = false
        player.foodDeposited += 1

        if (gameState.gameType != GameType.DemoGame && player.team != XYConstants.teamSideFromXCoordinate(x)) {
          info(s"Swapping team sides.  $player deposited a barry at coordinate $x, which belongs to the other team")
          val newRightTeam = XYConstants.LeftTeam
          XYConstants.LeftTeam = XYConstants.RightTeam
          XYConstants.RightTeam = newRightTeam
        }

        return Some(PlayerStateEnrichedEvent(player.currentState, e))

      case e@BerryKickInEvent(x, _, player, ownTeamOption) =>
        // TODO: Check if ownTeamOption above matches my calculations, otherwise adjust team side?
        if (XYConstants.teamSideFromXCoordinate(x) == player.team) {
          player.foodKickedInForMyTeam += 1
        } else {
          player.foodKickedInForOtherTeam += 1
        }

        return Some(PlayerStateEnrichedEvent(player.currentState, e))

      case e@CarryFoodEvent(player) =>
        player.currentState.hasFood = true

        return Some(PlayerStateEnrichedEvent(player.currentState, e))

      case PlayerKillEvent(_, _, _, victim, _) =>
        victim.currentState.hasFood = false

      case SpawnEvent(player, _) =>
        player.currentState.hasFood = false

      case UseMaidenEvent(_, _, _, player) =>
        player.currentState.hasFood = false

      case _ =>
    }

    None
  }

  /**
   * Sometimes these descriptors get redundant.
   */
  private[this] def updateSnailState(event: GameplayEvent): Option[EnrichedEvent] = {
    def recordSnailAt(newX: Int, player: Player): Unit = {
      player.distanceTraveledOnSnail += Math.abs(gameState.lastKnownSnailPosition - newX)
      gameState.lastKnownSnailPosition = newX
    }

    event match {
      case e@GetOffSnailEvent(x, _, player) =>
        player.currentState.isOnSnail = false
        recordSnailAt(x, player)

        return Some(PlayerStateEnrichedEvent(player.currentState, e))

      case e@GetOnSnailEvent(x, _, player) =>
        player.currentState.isOnSnail = true
        recordSnailAt(x, player)

        return Some(PlayerStateEnrichedEvent(player.currentState, e))

      case e@SnailEatEvent(x, _, rider, victim) =>
        recordSnailAt(x, rider)

        return Some(EnrichedSnailEatEvent(rider.currentState, victim.currentState, e))

      case e@SnailEscapeEvent(_, _, player) =>
        player.snailEscapes += 1

        return Some(PlayerStateEnrichedEvent(player.currentState, e))

      case PlayerKillEvent(x, _, _, victim, _) if victim.currentState.isOnSnail =>
        victim.currentState.isOnSnail = false
        recordSnailAt(x, victim)

      case VictoryEvent(_, "snail") =>
        val playerOnSnail = gameState.playerList.find(_.currentState.isOnSnail)

        playerOnSnail.foreach {
          player =>
            val x = XYConstants.snailGoalCoordinatesForTeam(player.team, gameState.map.get)._1
            player.distanceTraveledOnSnail += Math.abs(gameState.lastKnownSnailPosition - x)
        }

      case _ =>
    }

    None
  }

  private[this] def updateOtherStats(event: GameplayEvent): Option[EnrichedEvent] = {
    event match {
      case e@GlanceEvent(_, _, player1, player2) =>
        return Some(EnrichedGlanceEvent(player1.currentState, player2.currentState, e))

      case _ =>
    }

    None
  }

  private[this] def logEvent(event: Event): Unit = {
    allEvents.append(event)

    if (gameState.gameType != GameType.DemoGame) {
      info(eventSerializer.toJson(event))
    } else if (gameState.gameType == GameType.UnknownGame) {
      // Queue up the events to be recorded later.  We'll know quickly if this is a demo game or not
      logQueue.append(event)
    }
  }

  def logCurrentGameState(): Unit = synchronized {
    if (gameState.inProgress) {
      val gameEvent = gameState.toFinalGameState
      val json = eventSerializer.toJson(gameEvent)
      info(json.replace(gameEvent.event, "gameState"))
    }
  }

  def reset(triggerEvent: Event): Unit = synchronized {
    logEvent(gameState.toFinalGameState)

    gameState.playerList.foreach {
      playerState => logEvent(playerState.toFinalPlayerState)
    }

    gameState = new GameState
    lastSeenEvent = None
    logQueue = mutable.Buffer[Event]()
    allEvents = mutable.Buffer[Event]()
    eventSerializer = new EventJsonSerializer(gameState)
    debug(s"Reset state machine. Triggered by $triggerEvent")
  }

}
