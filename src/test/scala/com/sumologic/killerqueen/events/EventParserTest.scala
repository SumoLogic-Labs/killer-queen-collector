package com.sumologic.killerqueen.events

import com.sumologic.killerqueen.TestBase
import com.sumologic.killerqueen.model.InboundEvents._
import com.sumologic.killerqueen.model.{InboundEvent, Player}

import scala.collection.mutable

class EventParserTest extends TestBase {
  "EventParser" should {
    "Handle known input types" in {
      val eventAndResult = mutable.Buffer[(String, InboundEvent)]()
      def parseAndRecord(event: String): InboundEvent = {
        val result = EventParser.parse(event)
        eventAndResult.append((event, result))
        result
      }

      parseAndRecord("![k[alive],v[11:16:11 PM]]!") should be(AliveEvent("11:16:11 PM"))
      parseAndRecord("![k[connected],v[2]]!") should be(ConnectedEvent(2))
      parseAndRecord("![k[gameend],v[map_day,False,71.761,False]]!") should be(GameEndEvent("map_day", false, 71.761, false))
      parseAndRecord("![k[gamestart],v[map_day,False,0,False]]!") should be(GameStartEvent("map_day", false, 0, false))
      parseAndRecord("![k[victory],v[Blue,economic]]!") should be(VictoryEvent("Blue", "economic"))
      parseAndRecord("![k[userNameUpdate],v[Rambo,,Chris,,,,,,,]]!") should be(UserNameUpdateEvent("Rambo", "", "Chris", "", "", "", "", "", "", ""))

      parseAndRecord("![k[berryDeposit],v[884,990,4]]!") should be(BerryDepositEvent(884, 990, Player(4)))
      parseAndRecord("![k[berryKickIn],v[804,645,2]]!") should be(BerryKickInEvent(804, 645, Player(2)))
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
