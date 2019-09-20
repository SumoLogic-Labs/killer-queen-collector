package com.sumologic.killerqueen.events

import com.sumologic.killerqueen.model.{Event, Player, WireEvent}

import scala.util.matching.Regex

/**
 * Turns the KQ wire events into Scala objects that we can manipulate.
 *
 * See:
 * - https://www.reddit.com/r/KillerQueen/comments/82tbug/feature_request_stats/dzghx0t
 * - https://github.com/killer-queen-stats/kqstats/wiki/Socket-Messages
 */
object EventParser {

  private[this] def createRegex(key: String, valueRegex: String): Regex = {
    s"^!\\[k\\[$key\\],v\\[$valueRegex\\]\\]!$$".r
  }

  // Game controls
  private val Alive = createRegex("alive", "(.+?)") // ![k[alive],v[11:16:11 PM]]!
  private val Connected = createRegex("connected", "(\\d+)") // ![k[connected],v[2]]!
  private val GameEnd = createRegex("gameend", "([\\w_]+),(False|True),([\\d\\.]+),(False|True)") // ![k[gameend],v[map_day,False,71.761,False]]!
  private val GameStart = createRegex("gamestart", "([\\w_]+),(False|True),([\\d\\.]+),(False|True)") // ![k[gamestart],v[map_day,False,0,False]]!
  private val Victory = createRegex("victory", "(\\w+),(\\w+)") // ![k[victory],v[Blue,economic]]!
  private val UserNameUpdate = createRegex("userNameUpdate", "([^,]*?),([^,]*?),([^,]*?),([^,]*?),([^,]*?),([^,]*?),([^,]*?),([^,]*?),([^,]*?),([^,]*?)")
  private val Login = createRegex("loginevent", "([01])") // ![k[loginevent],v[1]]!

  // Berrys
  private val BerryDeposit = createRegex("berryDeposit", "(\\d+),(\\d+),(\\d+)") // ![k[berryDeposit],v[884,990,4]]!
  private val BerryKickIn = createRegex("berryKickIn", "(\\d+),(\\d+),(\\d+)") // ![k[berryKickIn],v[804,645,2]]!
  private val BerryKickIn2 = createRegex("berryKickIn", "(\\d+),(\\d+),(\\d+),?(True|False)") // ![k[berryKickIn],v[804,645,2True]]!
  private val CarryFood = createRegex("carryFood", "(\\d+)") // ![k[carryFood],v[10]]!

  // Maidens / gates
  private val BlessMaiden = createRegex("blessMaiden", "(\\d+),(\\d+),(\\w+)") // ![k[blessMaiden],v[1360,260,Blue]]!
  private val ReserveMaiden = createRegex("reserveMaiden", "(\\d+),(\\d+),(\\d+)") // ![k[reserveMaiden],v[310,620,4]]!
  private val UnreserveMaiden = createRegex("unreserveMaiden", "(\\d+),(\\d+),,(\\d+)") // ![k[unreserveMaiden],v[340,140,,5]]!
  private val UseMaiden = createRegex("useMaiden", "(\\d+),(\\d+),(\\w+),(\\d+)") // ![k[useMaiden],v[1580,140,maiden_speed,5]]!

  // Snails
  private val GetOffSnail = createRegex("getOffSnail: ", "(\\d+),(\\d+),,(\\d+)") // ![k[getOffSnail: ],v[1064,11,,3]]!
  private val GetOnSnail = createRegex("getOnSnail: ", "(\\d+),(\\d+),(\\d+)") // ![k[getOnSnail: ],v[1064,11,7]]!
  private val SnailEat = createRegex("snailEat", "(\\d+),(\\d+),(\\d+),(\\d+)") // ![k[snailEat],v[1081,11,7,10]]!
  private val SnailEscape = createRegex("snailEscape", "(\\d+),(\\d+),(\\d+)") // ![k[snailEscape],v[1131,11,10]]!

  // Players
  private val Glance = createRegex("glance", "(\\d+),(\\d+)") // ![k[glance],v[1,2]]!
  private val Glance2 = createRegex("glance", "(\\d+),(\\d+),(\\d+),(\\d+)") // ![k[glance],v[1229,294,1,2]]!
  private val PlayerKill = createRegex("playerKill", "(\\d+),(\\d+),(\\d+),(\\d+),(\\w+)") // ![k[playerKill],v[1006,20,10,5,Worker]]!
  private val PlayerNames = createRegex("playernames", ",,,,,,,,,") // ![k[playernames],v[,,,,,,,,,]]!
  private val Spawn = createRegex("spawn", "(\\d+),(\\w+)") // ![k[spawn],v[1,False]]!

  // Outbound events
  private val ImAlive = createRegex("im alive", "(.+?)") // ![k[im alive],v[null]]!
  private val Connect = createRegex("connect", "\\{\"name\":\"(.+?)\",\"isGameMachine\":(true|false)\\}") // "![k[connect],v[{\"name\":\"1\",\"isGameMachine\":false}]]!"
  private val AdminLogin = createRegex("adminlogin", "midwife") // ![k[adminlogin],v[midwife]]!
  private val GetConfig = createRegex("get", "(.+?)") // ![k[get],v[goldonleft]]!

  // Tournament events
  private val TournamentStatus = createRegex("tournamentstatus", "([01]),([01])") // ![k[tournamentstatus],v[0,0]]!
  private val TournamentBracket = createRegex("bracket", "(.+?)") // ![k[bracket],v[a ton of JSON here]]!
  private val TournamentStart = createRegex("tstart", "(.+?)") // ![k[tstart],v[a ton of JSON here]]!
  private val TournamentConcluded = createRegex("tournamentconcluded", "(.+?)") // ![k[tournamentconcluded],v[a ton of JSON here]]!
  private val MachineNames = createRegex("machinenames", "(.+?)") // ![k[machinenames],v[["KQ"]]]!
  private val MapSelect = createRegex("mapselect", "(.*?)") // ![k[mapselect],v[]]! or ![k[mapselect],v[midwife]]!
  private val RestartTourney = createRegex("restarttourney", "(.+?)") // ![k[restarttourney],v[r,asdf,,1,1,1,a,b,c]]!
  private val AssignToMachine = createRegex("assigntomachine", "(\\d+),(.+?)") // ![k[assigntomachine],v[3,KQ]]!

  // Catch all
  private val GeneralForm = createRegex("(.*?)", "(.*?)")


  def parse(event: String): WireEvent = {
    import com.sumologic.killerqueen.model.InboundEvents._
    import com.sumologic.killerqueen.model.OutboundEvents._

    event match {
      case Alive(timestamp) => AliveEvent(timestamp)
      case Connected(connectionId) => ConnectedEvent(connectionId.toInt)
      case GameEnd(map, unknown1, duration, unknown2) => GameEndEvent(map, unknown1.toBoolean, duration.toDouble, unknown2.toBoolean)
      case GameStart(map, unknown1, duration, unknown2) => GameStartEvent(map, unknown1.toBoolean, duration.toInt, unknown2.toBoolean)
      case Victory(team, tpe) => VictoryEvent(team, tpe)
      case UserNameUpdate(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) => UserNameUpdateEvent(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
      case Login(isSuccess) => LoginEvent(isSuccess == "1")

      case BerryDeposit(x, y, playerId) => BerryDepositEvent(x.toInt, y.toInt, Player(playerId.toInt))
      case BerryKickIn(x, y, playerId) => BerryKickInEvent(x.toInt, y.toInt, Player(playerId.toInt), None)
      case BerryKickIn2(x, y, playerId, ownTeam) => BerryKickInEvent(x.toInt, y.toInt, Player(playerId.toInt), Some(ownTeam.toBoolean))
      case CarryFood(playerId) => CarryFoodEvent(Player(playerId.toInt))

      case BlessMaiden(x, y, team) =>
        val adjustedTeam = if (team == "Red") "Gold" else team
        BlessMaidenEvent(x.toInt, y.toInt, adjustedTeam)
      case ReserveMaiden(x, y, playerId) => ReserveMaidenEvent(x.toInt, y.toInt, Player(playerId.toInt))
      case UnreserveMaiden(x, y, playerId) => UnreserveMaidenEvent(x.toInt, y.toInt, Player(playerId.toInt))
      case UseMaiden(x, y, tpe, playerId) => UseMaidenEvent(x.toInt, y.toInt, tpe, Player(playerId.toInt))

      case GetOffSnail(x, y, playerId) => GetOffSnailEvent(x.toInt, y.toInt, Player(playerId.toInt))
      case GetOnSnail(x, y, playerId) => GetOnSnailEvent(x.toInt, y.toInt, Player(playerId.toInt))
      case SnailEat(x, y, killerId, victimId) => SnailEatEvent(x.toInt, y.toInt, Player(killerId.toInt), Player(victimId.toInt))
      case SnailEscape(x, y, playerId) => SnailEscapeEvent(x.toInt, y.toInt, Player(playerId.toInt))

      case Glance(playerId1, playerId2) => GlanceEvent(None, None, Player(playerId1.toInt), Player(playerId2.toInt))
      case Glance2(x, y, playerId1, playerId2) => GlanceEvent(Some(x.toInt), Some(y.toInt), Player(playerId1.toInt), Player(playerId2.toInt))
      case PlayerKill(x, y, killerId, victimId, victimType) => PlayerKillEvent(x.toInt, y.toInt, Player(killerId.toInt), Player(victimId.toInt), victimType)
      case PlayerNames() => PlayerNamesEvent
      case Spawn(playerId, isBot) => SpawnEvent(Player(playerId.toInt), isBot.toBoolean)

      case ImAlive(text) => ImAliveEvent(text)
      case Connect(name, isGameMachine) => ConnectEvent(name, isGameMachine.toBoolean)
      case AdminLogin() => AdminLoginEvent
      case GetConfig(config) => GetConfigEvent(config)

      case TournamentStatus(isStarted, isConcluded) => TournamentStatusEvent(isStarted == "1", isConcluded == "1")
      case TournamentBracket(json) => TournamentBracketEvent(json)
      case TournamentStart(json) => TournamentStartEvent(json)
      case TournamentConcluded(json) => TournamentConcludedEvent(json)
      case MachineNames(names) => MachineNamesEvent(names)
      case MapSelect(password) => MapSelectEvent(password)
      case RestartTourney(value) => RestartTourneyEvent(value)
      case AssignToMachine(team, machine) => AssignToMachineEvent(team.toInt, machine)

      case GeneralForm(key, value) => UnknownEvent(key, value)
      case _ => throw new Exception(s"Unknown input: $event")
    }
  }

}


