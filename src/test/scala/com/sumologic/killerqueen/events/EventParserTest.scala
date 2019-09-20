package com.sumologic.killerqueen.events

import com.sumologic.killerqueen.TestBase
import com.sumologic.killerqueen.model.InboundEvents._
import com.sumologic.killerqueen.model.OutboundEvents._
import com.sumologic.killerqueen.model.{Player, WireEvent}

import scala.collection.mutable

class EventParserTest extends TestBase {
  "EventParser" should {
    "Handle known input types" in {
      val eventAndResult = mutable.Buffer[(String, WireEvent)]()

      def parseAndRecord(event: String): WireEvent = {
        val result = EventParser.parse(event)
        eventAndResult.append((event, result))
        result
      }

      def parseButDontRecord(event: String): WireEvent = {
        EventParser.parse(event)
      }

      parseAndRecord("![k[alive],v[11:16:11 PM]]!") should be(AliveEvent("11:16:11 PM"))
      parseAndRecord("![k[connected],v[2]]!") should be(ConnectedEvent(2))
      parseAndRecord("![k[gameend],v[map_day,False,71.761,False]]!") should be(GameEndEvent("map_day", false, 71.761, false))
      parseAndRecord("![k[gamestart],v[map_day,False,0,False]]!") should be(GameStartEvent("map_day", false, 0, false))
      parseAndRecord("![k[victory],v[Blue,economic]]!") should be(VictoryEvent("Blue", "economic"))
      parseAndRecord("![k[userNameUpdate],v[Rambo,,Chris,,,,,,,]]!") should be(UserNameUpdateEvent("Rambo", "", "Chris", "", "", "", "", "", "", ""))
      parseAndRecord("![k[loginevent],v[1]]!") should be(LoginEvent(true))
      parseAndRecord("![k[loginevent],v[0]]!") should be(LoginEvent(false))

      parseAndRecord("![k[berryDeposit],v[884,990,4]]!") should be(BerryDepositEvent(884, 990, Player(4)))
      parseAndRecord("![k[berryKickIn],v[804,645,2]]!") should be(BerryKickInEvent(804, 645, Player(2), None))
      parseButDontRecord("![k[berryKickIn],v[804,645,2True]]!") should be(BerryKickInEvent(804, 645, Player(2), Some(true))) // Temporarily broken format from some APIs
      parseAndRecord("![k[berryKickIn],v[804,645,2,True]]!") should be(BerryKickInEvent(804, 645, Player(2), Some(true)))
      parseAndRecord("![k[carryFood],v[10]]!") should be(CarryFoodEvent(Player(10)))

      parseAndRecord("![k[blessMaiden],v[1360,260,Blue]]!") should be(BlessMaidenEvent(1360, 260, "Blue"))
      parseAndRecord("![k[blessMaiden],v[1360,260,Red]]!") should be(BlessMaidenEvent(1360, 260, "Gold"))
      parseAndRecord("![k[reserveMaiden],v[310,620,4]]!") should be(ReserveMaidenEvent(310, 620, Player(4)))
      parseAndRecord("![k[unreserveMaiden],v[340,140,,5]]!") should be(UnreserveMaidenEvent(340, 140, Player(5)))
      parseAndRecord("![k[useMaiden],v[1580,140,maiden_speed,5]]!") should be(UseMaidenEvent(1580, 140, "maiden_speed", Player(5)))

      parseAndRecord("![k[getOffSnail: ],v[1064,11,,3]]!") should be(GetOffSnailEvent(1064, 11, Player(3)))
      parseAndRecord("![k[getOnSnail: ],v[1064,11,7]]!") should be(GetOnSnailEvent(1064, 11, Player(7)))
      parseAndRecord("![k[snailEat],v[1081,11,7,10]]!") should be(SnailEatEvent(1081, 11, Player(7), Player(10)))
      parseAndRecord("![k[snailEscape],v[1131,11,10]]!") should be(SnailEscapeEvent(1131, 11, Player(10)))

      parseAndRecord("![k[glance],v[1,2]]!") should be(GlanceEvent(None, None, Player(1), Player(2)))
      parseAndRecord("![k[glance],v[1229,294,1,2]]!") should be(GlanceEvent(Some(1229), Some(294), Player(1), Player(2)))
      parseAndRecord("![k[playerKill],v[1006,20,10,5,Worker]]!") should be(PlayerKillEvent(1006, 20, Player(10), Player(5), "Worker"))
      parseAndRecord("![k[playernames],v[,,,,,,,,,]]!") should be(PlayerNamesEvent)
      parseAndRecord("![k[spawn],v[1,False]]!") should be(SpawnEvent(Player(1), false))

      parseAndRecord("![k[im alive],v[null2]]!") should be(ImAliveEvent("null2"))
      parseAndRecord("![k[connect],v[{\"name\":\"1\",\"isGameMachine\":false}]]!") should be(ConnectEvent("1", false))
      parseAndRecord("![k[adminlogin],v[midwife]]!") should be(AdminLoginEvent)
      parseAndRecord("![k[get],v[goldonleft]]!") should be(GetConfigEvent("goldonleft"))

      parseAndRecord("![k[tournamentstatus],v[0,0]]!") should be(TournamentStatusEvent(false, false))
      parseAndRecord("![k[tournamentstatus],v[1,1]]!") should be(TournamentStatusEvent(true, true))
      parseAndRecord(
        "![k[bracket],v[{\"tournament_name\":\"asdf\",\"tournament_location\":\"\",\"format\":\"r\",\"concluded\":false,\"stream_url\":\"\",\"brackets\":[{\"bracket_type\":\"r\",\"rounds\":[{\"round_num\":0,\"sets\":[{\"id\":-1,\"best_of\":1,\"on_machine_name\":\"\",\"start_time\":\"\",\"teams\":[{\"name\":\"a\",\"score\":0},{\"name\":\"#FREE_WIN#\",\"score\":0}],\"victory_types\":[],\"match_data_id\":[]},{\"id\":1,\"best_of\":1,\"on_machine_name\":\"\",\"start_time\":\"\",\"teams\":[{\"name\":\"b\",\"score\":0},{\"name\":\"c\",\"score\":0}],\"victory_types\":[],\"match_data_id\":[]}]},{\"round_num\":1,\"sets\":[{\"id\":2,\"best_of\":1,\"on_machine_name\":\"\",\"start_time\":\"\",\"teams\":[{\"name\":\"c\",\"score\":0},{\"name\":\"a\",\"score\":0}],\"victory_types\":[],\"match_data_id\":[]},{\"id\":-1,\"best_of\":1,\"on_machine_name\":\"\",\"start_time\":\"\",\"teams\":[{\"name\":\"b\",\"score\":0},{\"name\":\"#FREE_WIN#\",\"score\":0}],\"victory_types\":[],\"match_data_id\":[]}]},{\"round_num\":2,\"sets\":[{\"id\":3,\"best_of\":1,\"on_machine_name\":\"\",\"start_time\":\"\",\"teams\":[{\"name\":\"a\",\"score\":0},{\"name\":\"b\",\"score\":0}],\"victory_types\":[],\"match_data_id\":[]},{\"id\":-1,\"best_of\":1,\"on_machine_name\":\"\",\"start_time\":\"\",\"teams\":[{\"name\":\"c\",\"score\":0},{\"name\":\"#FREE_WIN#\",\"score\":0}],\"victory_types\":[],\"match_data_id\":[]}]}]}],\"standings\":[{\"name\":\"a\",\"point\":0,\"win\":0,\"loss\":0},{\"name\":\"b\",\"point\":0,\"win\":0,\"loss\":0},{\"name\":\"c\",\"point\":0,\"win\":0,\"loss\":0}]}]]!"
      ) should be(TournamentBracketEvent("{\"tournament_name\":\"asdf\",\"tournament_location\":\"\",\"format\":\"r\",\"concluded\":false,\"stream_url\":\"\",\"brackets\":[{\"bracket_type\":\"r\",\"rounds\":[{\"round_num\":0,\"sets\":[{\"id\":-1,\"best_of\":1,\"on_machine_name\":\"\",\"start_time\":\"\",\"teams\":[{\"name\":\"a\",\"score\":0},{\"name\":\"#FREE_WIN#\",\"score\":0}],\"victory_types\":[],\"match_data_id\":[]},{\"id\":1,\"best_of\":1,\"on_machine_name\":\"\",\"start_time\":\"\",\"teams\":[{\"name\":\"b\",\"score\":0},{\"name\":\"c\",\"score\":0}],\"victory_types\":[],\"match_data_id\":[]}]},{\"round_num\":1,\"sets\":[{\"id\":2,\"best_of\":1,\"on_machine_name\":\"\",\"start_time\":\"\",\"teams\":[{\"name\":\"c\",\"score\":0},{\"name\":\"a\",\"score\":0}],\"victory_types\":[],\"match_data_id\":[]},{\"id\":-1,\"best_of\":1,\"on_machine_name\":\"\",\"start_time\":\"\",\"teams\":[{\"name\":\"b\",\"score\":0},{\"name\":\"#FREE_WIN#\",\"score\":0}],\"victory_types\":[],\"match_data_id\":[]}]},{\"round_num\":2,\"sets\":[{\"id\":3,\"best_of\":1,\"on_machine_name\":\"\",\"start_time\":\"\",\"teams\":[{\"name\":\"a\",\"score\":0},{\"name\":\"b\",\"score\":0}],\"victory_types\":[],\"match_data_id\":[]},{\"id\":-1,\"best_of\":1,\"on_machine_name\":\"\",\"start_time\":\"\",\"teams\":[{\"name\":\"c\",\"score\":0},{\"name\":\"#FREE_WIN#\",\"score\":0}],\"victory_types\":[],\"match_data_id\":[]}]}]}],\"standings\":[{\"name\":\"a\",\"point\":0,\"win\":0,\"loss\":0},{\"name\":\"b\",\"point\":0,\"win\":0,\"loss\":0},{\"name\":\"c\",\"point\":0,\"win\":0,\"loss\":0}]}"))
      parseAndRecord(
        "![k[tstart],v[{\"start\":\"True\",\"format\":\"r\",\"tournamentname\":\"asdf\",\"message\":\"Can not connect to Database, bracket will only be locally saved.\"}]]!"
      ) should be(TournamentStartEvent("{\"start\":\"True\",\"format\":\"r\",\"tournamentname\":\"asdf\",\"message\":\"Can not connect to Database, bracket will only be locally saved.\"}"))
      parseAndRecord(
        "![k[tournamentconcluded],v[[{\"name\":\"a\",\"point\":1,\"win\":1,\"loss\":1},{\"name\":\"b\",\"point\":1,\"win\":1,\"loss\":1},{\"name\":\"c\",\"point\":1,\"win\":1,\"loss\":1}]]]!"
      ) should be(TournamentConcludedEvent("[{\"name\":\"a\",\"point\":1,\"win\":1,\"loss\":1},{\"name\":\"b\",\"point\":1,\"win\":1,\"loss\":1},{\"name\":\"c\",\"point\":1,\"win\":1,\"loss\":1}]"))
      parseAndRecord("![k[machinenames],v[[\"KQ\"]]]!") should be(MachineNamesEvent("[\"KQ\"]"))

      parseAndRecord("![k[mapselect],v[]]!") should be(MapSelectEvent(""))
      parseAndRecord("![k[mapselect],v[midwife]]!") should be(MapSelectEvent("midwife"))
      parseAndRecord("![k[restarttourney],v[r,asdf,,1,1,1,a,b,c]]!") should be(RestartTourneyEvent("r,asdf,,1,1,1,a,b,c"))
      parseAndRecord("![k[assigntomachine],v[3,KQ]]!") should be(AssignToMachineEvent(3, "KQ"))

      intercept[Exception] {
        EventParser.parse("asdf")
      }

      parseAndRecord("![k[a],v[b]]!") should be(UnknownEvent("a", "b"))


      // All events ran should return the input string
      eventAndResult.foreach {
        case (event, result) =>
          result.toApi should be(event)
      }
    }
  }

}
