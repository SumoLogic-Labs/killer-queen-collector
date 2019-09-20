package com.sumologic.killerqueen.state

import com.sumologic.killerqueen.events.{EventHandler, EventParser, NoopEventSender}
import com.sumologic.killerqueen.model.InboundEvents._
import com.sumologic.killerqueen.model.{GameplayEvent, Player, XYConstants}
import com.sumologic.killerqueen.{Logging, TestBase}
import org.scalatest.BeforeAndAfterEach

class StateMachineTest extends TestBase with BeforeAndAfterEach with Logging {

  "StateMachine" should {
    "correctly recognize regular games" when {
      "they're fresh and standalone" in {
        parseAndRunEvents(regularGame)

        sut.gameState.gameType should be(GameType.RegularGame)
      }

      "they follow a demo game" in {
        parseAndRunEvents(firstDemoGameScript)
        parseAndRunEvents(regularGame)

        sut.gameState.gameType should be(GameType.RegularGame)
      }
    }

    "correctly recognize demo games" when {
      "its script one" in {
        parseAndRunEvents(firstDemoGameScript)
        sut.gameState.gameType should be(GameType.DemoGame)
      }

      "its script two" in {
        parseAndRunEvents(secondDemoGameScript)
        sut.gameState.gameType should be(GameType.DemoGame)
      }
    }

    "correctly recognize bonus games" when {

      "military victory happens with two queen kills" in {
        parseAndRunEvents(bonusGameUnknownStart)

        sut.gameState.gameType should be(GameType.UnknownGame)

        sut.processEvent(PlayerKillEvent(0, 0, Player(1), Player(2), "Queen"))
        sut.processEvent(PlayerKillEvent(0, 0, Player(1), Player(2), "Queen"))
        sut.processEvent(GameEndEvent("map_dusk", false, 20.07644, false))
        sut.processEvent(VictoryEvent("Gold", "military"))

        // Check for spawning as fast warrior after bonus game is detected
        sut.processEvent(SpawnEvent(Player(3), false))
        Player(3).currentState.isWarrior should be(true)
        Player(3).currentState.isFast should be(true)

        sut.gameState.gameType should be(GameType.MilitaryBonusGame)
      }

      "an unexpected soldier kills someone" in {
        parseAndRunEvents(bonusGameUnknownStart)

        Player(4).currentState.isWarrior should be(false)

        sut.processEvent(PlayerKillEvent(0, 0, Player(3), Player(1), "Queen"))
        sut.gameState.gameType should be(GameType.MilitaryBonusGame)

        Player(1).currentState.isFast should be(false)
        Player(2).currentState.isFast should be(true) // Got promoted

        // Check for promotion of already spawned character to fast warrior (and not even the one who made the kill!)
        Player(4).currentState.isWarrior should be(true)
        Player(4).currentState.isFast should be(true)
      }

      "queen kills an unexpected soldier" in {
        parseAndRunEvents(bonusGameUnknownStart)
        sut.processEvent(PlayerKillEvent(0, 0, Player(1), Player(3), "Soldier"))
        sut.gameState.gameType should be(GameType.MilitaryBonusGame)
      }
    }

    "correctly process entire game transcript" in {
      parseAndRunEvents(regularGame)

      sut.gameState.inProgress should be(false)
      sut.gameState.victor should be(Some("Gold"))
      sut.gameState.winType should be(Some("military"))
      sut.gameState.duration should be(Some(120.7771))
      sut.gameState.map should be(Some("map_dusk"))

      // Check k/d
      Player(1).totalKills should be(11)
      Player(2).totalKills should be(5)
      Player(3).totalKills should be(0)
      Player(4).totalKills should be(1)
      Player(5).totalKills should be(5)
      Player(6).totalKills should be(4)
      Player(7).totalKills should be(1)
      Player(8).totalKills should be(6)
      Player(9).totalKills should be(0)
      Player(10).totalKills should be(9)

      Player(1).totalDeaths should be(1)
      Player(2).totalDeaths should be(3)
      Player(3).totalDeaths should be(7)
      Player(4).totalDeaths should be(6)
      Player(5).totalDeaths should be(3)
      Player(6).totalDeaths should be(4)
      Player(7).totalDeaths should be(10)
      Player(8).totalDeaths should be(2)
      Player(9).totalDeaths should be(4)
      Player(10).totalDeaths should be(2)

      sut.gameState.playerList.map(_.totalDeaths).sum should be(sut.gameState.playerList.map(_.totalKills).sum)

      // Check berries
      Player(3).foodDeposited should be(3)
      Player(4).foodDeposited should be(4)
      Player(5).foodDeposited should be(0)
      Player(6).foodDeposited should be(4)
      Player(7).foodDeposited should be(0)
      Player(8).foodDeposited should be(0)
      Player(9).foodDeposited should be(4)
      Player(10).foodDeposited should be(1)

      Player(1).foodKickedInForMyTeam should be(0)
      Player(2).foodKickedInForMyTeam should be(0)
      Player(3).foodKickedInForMyTeam should be(0)
      Player(4).foodKickedInForMyTeam should be(0)
      Player(5).foodKickedInForMyTeam should be(0)
      Player(6).foodKickedInForMyTeam should be(0)
      Player(7).foodKickedInForMyTeam should be(0)
      Player(8).foodKickedInForMyTeam should be(1)
      Player(9).foodKickedInForMyTeam should be(0)
      Player(10).foodKickedInForMyTeam should be(0)

      Player(1).foodKickedInForOtherTeam should be(0)
      Player(2).foodKickedInForOtherTeam should be(0)
      Player(3).foodKickedInForOtherTeam should be(0)
      Player(4).foodKickedInForOtherTeam should be(0)
      Player(5).foodKickedInForOtherTeam should be(0)
      Player(6).foodKickedInForOtherTeam should be(0)
      Player(7).foodKickedInForOtherTeam should be(0)
      Player(8).foodKickedInForOtherTeam should be(0)
      Player(9).foodKickedInForOtherTeam should be(0)
      Player(10).foodKickedInForOtherTeam should be(1)
    }

    "never enter an illegal state during complete game logs" when {
      def testResource(file: String): Unit = {
        // Based off https://stackoverflow.com/a/51686476
        val fileStream = getClass.getResourceAsStream(file)
        val lines = scala.io.Source.fromInputStream(fileStream).getLines.mkString("\n")
        parseAndRunEvents(lines, useHandler = true)
        sut.gamesPlayed should be > 0
        sut.exceptionFound should be(false)
      }

      "its the 18th" in {
        testResource("/raw_event_log_2018-09-18.txt")
      }

      "its the 19th" in {
        testResource("/raw_event_log_2018-09-19.txt")
      }

      "its the 20th" in {
        testResource("/raw_event_log_2018-09-20.txt")
      }

      "its the 21st" in {
        testResource("/raw_event_log_2018-09-21.txt")
      }

      "its the 26th" in {
        testResource("/raw_event_log_2018-09-26.txt")
      }

      "its the 28th" in {
        testResource("/raw_event_log_2018-09-28.txt")
      }

      "its Oct 1st" in {
        testResource("/raw_event_log_2018-10-01.txt")
      }

      "its Oct 2nd" in {
        testResource("/raw_event_log_2018-10-02.txt")
      }

      "its Oct 3rd" in {
        testResource("/raw_event_log_2018-10-03.txt")
      }

      "its Oct 4th" in {
        testResource("/raw_event_log_2018-10-04.txt")
      }

      "its Oct 5th" in {
        testResource("/raw_event_log_2018-10-05.txt")
      }

      "its Oct 22nd" in {
        testResource("/raw_event_log_2018-10-22.txt")
      }

      "its August 16th 2019" in {
        // This is actually a huge pile of days, both release and beta builds
        testResource("/raw_event_log_2019-08-16.txt")
      }

      "its September 20th 2019" in {
        testResource("/raw_event_log_2019-09-20.txt")
      }
    }

    "handle connecting part way through a game" in {
      val randomGameScript =
        """
          | IN: ![k[glance],v[5,8]]!
        """.stripMargin

      parseAndRunEvents(randomGameScript)
      sut.lastSeenEvent should be(None) // Never updated last event
    }

    "handle SnailEatEvent appropriately" in {
      // Initially I mistook the SnailEatEvent as a kill - it is not.  This fixed those cases
      //
      // ESCAPE CASE:
      // IN: ![k[snailEat],v[1800,11,9,6]]!
      // IN: ![k[snailEscape],v[1850,11,6]]!
      //
      // KILL CASE:
      // IN: ![k[snailEat],v[1706,11,9,10]]!
      // IN: ![k[playerKill],v[1776,20,9,10,Worker]]!

      parseAndRunEvents(regularGameStart)
      sut.processEvent(GetOnSnailEvent(0, 0, Player(3)))

      Player(4).currentState.isFast = true
      Player(4).currentState.hasFood = true
      sut.processEvent(SnailEatEvent(0, 0, Player(3), Player(4)))

      Player(4).currentState.isFast should be(true)
      Player(4).currentState.hasFood should be(true)

      sut.processEvent(PlayerKillEvent(0, 0, Player(3), Player(4), "Worker"))
      Player(4).currentState.isFast should be(false)
      Player(4).currentState.hasFood should be(false)
    }
  }

  private[this] val regularGame =
    """ IN: ![k[spawn],v[4,False]]!
      | IN: ![k[spawn],v[10,False]]!
      | IN: ![k[spawn],v[3,False]]!
      | IN: ![k[spawn],v[1,False]]!
      | IN: ![k[spawn],v[2,False]]!
      | IN: ![k[playernames],v[,,,,,,,,,]]!
      | IN: ![k[spawn],v[9,False]]!
      | IN: ![k[spawn],v[5,False]]!
      | IN: ![k[spawn],v[6,False]]!
      | IN: ![k[spawn],v[7,True]]!
      | IN: ![k[spawn],v[8,False]]!
      | IN: ![k[gamestart],v[map_dusk,False,0,False]]!
      | IN: ![k[blessMaiden],v[960,140,Red]]!
      | IN: ![k[carryFood],v[6]]!
      | IN: ![k[blessMaiden],v[310,620,Blue]]!
      | IN: ![k[carryFood],v[5]]!
      | IN: ![k[carryFood],v[9]]!
      | IN: ![k[spawn],v[8,False]]!
      | IN: ![k[reserveMaiden],v[1610,620,6]]!
      | IN: ![k[carryFood],v[4]]!
      | IN: ![k[reserveMaiden],v[1580,140,5]]!
      | IN: ![k[blessMaiden],v[310,620,Red]]!
      | IN: ![k[glance],v[4,7]]!
      | IN: ![k[getOnSnail: ],v[960,11,4]]!
      | IN: ![k[snailEat],v[948,11,4,7]]!
      | IN: ![k[blessMaiden],v[310,620,Blue]]!
      | IN: ![k[useMaiden],v[1610,620,maiden_wings,6]]!
      | IN: ![k[carryFood],v[8]]!
      | IN: ![k[playerKill],v[379,624,2,3,Worker]]!
      | IN: ![k[useMaiden],v[1580,140,maiden_speed,5]]!
      | IN: ![k[blessMaiden],v[340,140,Red]]!
      | IN: ![k[carryFood],v[10]]!
      | IN: ![k[berryDeposit],v[1112,712,9]]!
      | IN: ![k[carryFood],v[5]]!
      | IN: ![k[reserveMaiden],v[1580,140,10]]!
      | IN: ![k[playerKill],v[964,20,4,7,Worker]]!
      | IN: ![k[reserveMaiden],v[310,620,8]]!
      | IN: ![k[blessMaiden],v[960,140,Blue]]!
      | IN: ![k[blessMaiden],v[310,620,Red]]!
      | IN: ![k[useMaiden],v[1580,140,maiden_speed,10]]!
      | IN: ![k[useMaiden],v[310,620,maiden_wings,8]]!
      | IN: ![k[carryFood],v[10]]!
      | IN: ![k[blessMaiden],v[1610,620,Blue]]!
      | IN: ![k[carryFood],v[9]]!
      | IN: ![k[glance],v[10,5]]!
      | IN: ![k[glance],v[9,10]]!
      | IN: ![k[glance],v[9,10]]!
      | IN: ![k[carryFood],v[3]]!
      | IN: ![k[blessMaiden],v[960,140,Red]]!
      | IN: ![k[getOffSnail: ],v[831,11,,4]]!
      | IN: ![k[playerKill],v[831,11,1,4,Worker]]!
      | IN: ![k[playerKill],v[1036,111,6,3,Worker]]!
      | IN: ![k[getOnSnail: ],v[831,11,7]]!
      | IN: ![k[playerKill],v[1428,20,2,5,Worker]]!
      | IN: ![k[getOffSnail: ],v[873,11,,7]]!
      | IN: ![k[playerKill],v[873,11,8,7,Worker]]!
      | IN: ![k[blessMaiden],v[1580,140,Blue]]!
      | IN: ![k[reserveMaiden],v[1610,620,10]]!
      | IN: ![k[glance],v[8,9]]!
      | IN: ![k[glance],v[8,9]]!
      | IN: ![k[glance],v[6,9]]!
      | IN: ![k[useMaiden],v[1610,620,maiden_wings,10]]!
      | IN: ![k[carryFood],v[3]]!
      | IN: ![k[carryFood],v[4]]!
      | IN: ![k[blessMaiden],v[1610,620,Red]]!
      | IN: ![k[carryFood],v[5]]!
      | IN: ![k[blessMaiden],v[1610,620,Blue]]!
      | IN: ![k[berryDeposit],v[1161,589,3]]!
      | IN: ![k[reserveMaiden],v[340,140,5]]!
      | IN: ![k[blessMaiden],v[1580,140,Red]]!
      | IN: ![k[playerKill],v[1062,20,8,7,Worker]]!
      | IN: ![k[berryDeposit],v[765,580,4]]!
      | IN: ![k[useMaiden],v[340,140,maiden_speed,5]]!
      | IN: ![k[berryDeposit],v[1119,612,9]]!
      | IN: ![k[carryFood],v[5]]!
      | IN: ![k[playerKill],v[1379,445,10,3,Worker]]!
      | IN: ![k[blessMaiden],v[1580,140,Blue]]!
      | IN: ![k[getOnSnail: ],v[873,11,4]]!
      | IN: ![k[playerKill],v[1048,70,6,7,Worker]]!
      | IN: ![k[reserveMaiden],v[310,620,5]]!
      | IN: ![k[glance],v[8,9]]!
      | IN: ![k[getOffSnail: ],v[819,11,,4]]!
      | IN: ![k[playerKill],v[819,11,1,4,Worker]]!
      | IN: ![k[carryFood],v[9]]!
      | IN: ![k[useMaiden],v[310,620,maiden_wings,5]]!
      | IN: ![k[blessMaiden],v[960,140,Blue]]!
      | IN: ![k[carryFood],v[3]]!
      | IN: ![k[playerKill],v[930,140,1,4,Worker]]!
      | IN: ![k[getOnSnail: ],v[819,11,7]]!
      | IN: ![k[blessMaiden],v[960,140,Red]]!
      | IN: ![k[glance],v[8,5]]!
      | IN: ![k[blessMaiden],v[310,620,Blue]]!
      | IN: ![k[getOffSnail: ],v[849,11,,7]]!
      | IN: ![k[playerKill],v[849,11,6,7,Worker]]!
      | IN: ![k[carryFood],v[3]]!
      | IN: ![k[blessMaiden],v[1610,620,Red]]!
      | IN: ![k[playerKill],v[1659,868,10,3,Worker]]!
      | IN: ![k[playerKill],v[1167,634,6,9,Worker]]!
      | IN: ![k[carryFood],v[4]]!
      | IN: ![k[playerKill],v[1079,974,5,6,Soldier]]!
      | IN: ![k[playerKill],v[1002,987,5,2,Queen]]!
      | IN: ![k[getOnSnail: ],v[849,11,7]]!
      | IN: ![k[playerKill],v[1646,620,10,3,Worker]]!
      | IN: ![k[glance],v[10,1]]!
      | IN: ![k[playerKill],v[1473,931,5,8,Soldier]]!
      | IN: ![k[carryFood],v[9]]!
      | IN: ![k[carryFood],v[6]]!
      | IN: ![k[berryDeposit],v[777,743,4]]!
      | IN: ![k[carryFood],v[3]]!
      | IN: ![k[blessMaiden],v[960,140,Blue]]!
      | IN: ![k[carryFood],v[8]]!
      | IN: ![k[reserveMaiden],v[960,140,8]]!
      | IN: ![k[getOffSnail: ],v[986,11,,7]]!
      | IN: ![k[playerKill],v[986,11,2,7,Worker]]!
      | IN: ![k[berryDeposit],v[1185,612,9]]!
      | IN: ![k[berryDeposit],v[803,612,6]]!
      | IN: ![k[blessMaiden],v[310,620,Red]]!
      | IN: ![k[glance],v[3,10]]!
      | IN: ![k[getOnSnail: ],v[986,11,4]]!
      | IN: ![k[useMaiden],v[960,140,maiden_wings,8]]!
      | IN: ![k[playerKill],v[1473,801,10,3,Worker]]!
      | IN: ![k[playerKill],v[1077,1000,5,2,Queen]]!
      | IN: ![k[blessMaiden],v[960,140,Red]]!
      | IN: ![k[getOffSnail: ],v[921,11,,4]]!
      | IN: ![k[playerKill],v[921,11,1,4,Worker]]!
      | IN: ![k[playerKill],v[1045,978,10,5,Soldier]]!
      | IN: ![k[carryFood],v[6]]!
      | IN: ![k[getOnSnail: ],v[921,11,7]]!
      | IN: ![k[carryFood],v[9]]!
      | IN: ![k[blessMaiden],v[960,140,Blue]]!
      | IN: ![k[glance],v[1,6]]!
      | IN: ![k[carryFood],v[3]]!
      | IN: ![k[getOffSnail: ],v[974,11,,7]]!
      | IN: ![k[playerKill],v[974,11,8,7,Worker]]!
      | IN: ![k[carryFood],v[5]]!
      | IN: ![k[playerKill],v[565,919,1,6,Worker]]!
      | IN: ![k[carryFood],v[4]]!
      | IN: ![k[berryDeposit],v[1148,751,3]]!
      | IN: ![k[reserveMaiden],v[310,620,5]]!
      | IN: ![k[playerKill],v[1884,782,10,9,Worker]]!
      | IN: ![k[blessMaiden],v[1610,620,Blue]]!
      | IN: ![k[getOnSnail: ],v[974,11,7]]!
      | IN: ![k[berryKickIn],v[1147,685,10]]!
      | IN: ![k[blessMaiden],v[960,140,Red]]!
      | IN: ![k[useMaiden],v[310,620,maiden_wings,5]]!
      | IN: ![k[carryFood],v[6]]!
      | IN: ![k[carryFood],v[3]]!
      | IN: ![k[blessMaiden],v[960,140,Blue]]!
      | IN: ![k[blessMaiden],v[1610,620,Red]]!
      | IN: ![k[getOffSnail: ],v[1040,11,,7]]!
      | IN: ![k[playerKill],v[1040,11,2,7,Worker]]!
      | IN: ![k[playerKill],v[565,982,10,5,Soldier]]!
      | IN: ![k[berryDeposit],v[834,672,6]]!
      | IN: ![k[berryDeposit],v[809,703,4]]!
      | IN: ![k[blessMaiden],v[1580,140,Red]]!
      | IN: ![k[glance],v[2,3]]!
      | IN: ![k[playerKill],v[1148,914,10,3,Worker]]!
      | IN: ![k[carryFood],v[9]]!
      | IN: ![k[playerKill],v[1661,870,10,9,Worker]]!
      | IN: ![k[getOnSnail: ],v[1040,11,7]]!
      | IN: ![k[carryFood],v[5]]!
      | IN: ![k[snailEat],v[1049,11,7,4]]!
      | IN: ![k[carryFood],v[6]]!
      | IN: ![k[reserveMaiden],v[1580,140,5]]!
      | IN: ![k[berryKickIn],v[739,648,8]]!
      | IN: ![k[playerKill],v[1032,20,7,4,Worker]]!
      | IN: ![k[blessMaiden],v[960,140,Red]]!
      | IN: ![k[useMaiden],v[1580,140,maiden_speed,5]]!
      | IN: ![k[glance],v[8,3]]!
      | IN: ![k[glance],v[2,3]]!
      | IN: ![k[berryDeposit],v[732,712,6]]!
      | IN: ![k[carryFood],v[5]]!
      | IN: ![k[carryFood],v[9]]!
      | IN: ![k[playerKill],v[1021,984,1,10,Soldier]]!
      | IN: ![k[blessMaiden],v[1610,620,Blue]]!
      | IN: ![k[getOffSnail: ],v[1140,11,,7]]!
      | IN: ![k[playerKill],v[1140,11,8,7,Worker]]!
      | IN: ![k[glance],v[2,9]]!
      | IN: ![k[playerKill],v[840,20,1,4,Worker]]!
      | IN: ![k[glance],v[6,5]]!
      | IN: ![k[glance],v[5,6]]!
      | IN: ![k[carryFood],v[3]]!
      | IN: ![k[blessMaiden],v[1580,140,Blue]]!
      | IN: ![k[glance],v[7,8]]!
      | IN: ![k[glance],v[2,3]]!
      | IN: ![k[playerKill],v[677,986,1,10,Worker]]!
      | IN: ![k[carryFood],v[6]]!
      | IN: ![k[carryFood],v[4]]!
      | IN: ![k[playerKill],v[1093,20,8,7,Worker]]!
      | IN: ![k[berryDeposit],v[1080,672,3]]!
      | IN: ![k[glance],v[2,9]]!
      | IN: ![k[berryDeposit],v[1085,749,9]]!
      | IN: ![k[blessMaiden],v[960,140,Blue]]!
      | IN: ![k[glance],v[5,6]]!
      | IN: ![k[playerKill],v[318,680,1,6,Worker]]!
      | IN: ![k[carryFood],v[10]]!
      | IN: ![k[getOnSnail: ],v[1140,11,7]]!
      | IN: ![k[playerKill],v[585,996,8,1,Queen]]!
      | IN: ![k[carryFood],v[3]]!
      | IN: ![k[carryFood],v[9]]!
      | IN: ![k[carryFood],v[6]]!
      | IN: ![k[berryDeposit],v[761,641,10]]!
      | IN: ![k[berryDeposit],v[826,747,4]]!
      | IN: ![k[blessMaiden],v[960,140,Red]]!
      | IN: ![k[glance],v[5,6]]!
      | IN: ![k[reserveMaiden],v[310,620,5]]!
      | IN: ![k[unreserveMaiden],v[310,620,,5]]!
      | IN: ![k[playerKill],v[1472,744,2,9,Worker]]!
      | IN: ![k[carryFood],v[10]]!
      | IN: ![k[glance],v[10,3]]!
      | IN: ![k[glance],v[3,10]]!
      | IN: ![k[reserveMaiden],v[960,140,5]]!
      | IN: ![k[playerKill],v[558,807,1,6,Worker]]!
      | IN: ![k[playerKill],v[615,917,1,8,Soldier]]!
      | IN: ![k[carryFood],v[4]]!
      | IN: ![k[carryFood],v[9]]!
      | IN: ![k[useMaiden],v[960,140,maiden_wings,5]]!
      | IN: ![k[glance],v[1,2]]!
      | IN: ![k[carryFood],v[6]]!
      | IN: ![k[glance],v[2,1]]!
      | IN: ![k[berryDeposit],v[822,589,6]]!
      | IN: ![k[playerKill],v[1061,927,5,2,Queen]]!
      | IN: ![k[gameend],v[map_dusk,False,120.7771,False]]!
      | IN: ![k[victory],v[Gold,military]]!""".stripMargin

  private[this] val firstDemoGameScript =
    """ IN: ![k[spawn],v[3,False]]!
      | IN: ![k[spawn],v[5,False]]!
      | IN: ![k[spawn],v[1,False]]!
      | IN: ![k[spawn],v[7,False]]!
      | IN: ![k[spawn],v[9,False]]!
      | IN: ![k[spawn],v[10,False]]!
      | IN: ![k[spawn],v[6,False]]!
      | IN: ![k[spawn],v[2,False]]!
      | IN: ![k[spawn],v[4,False]]!
      | IN: ![k[spawn],v[8,False]]!
      | IN: ![k[playernames],v[,,,,,,,,,]]!
      | IN: ![k[carryFood],v[5]]!
      | IN: ![k[carryFood],v[8]]!
      | IN: ![k[blessMaiden],v[1720,140,Blue]]!
      | IN: ![k[glance],v[1455,902,5,8]]!
      | IN: ![k[playerKill],v[1495,897,5,8,Soldier]]!""".stripMargin

  private[this] val secondDemoGameScript =
    """IN: ![k[spawn],v[1,False]]!
      | IN: ![k[spawn],v[2,False]]!
      | IN: ![k[playernames],v[,,,,,,,,,]]!
      | IN: ![k[spawn],v[5,False]]!
      | IN: ![k[spawn],v[7,False]]!
      | IN: ![k[spawn],v[6,False]]!
      | IN: ![k[spawn],v[3,False]]!
      | IN: ![k[spawn],v[9,False]]!
      | IN: ![k[spawn],v[8,False]]!
      | IN: ![k[spawn],v[4,False]]!
      | IN: ![k[spawn],v[10,False]]!
      | IN: ![k[carryFood],v[5]]!
      | IN: ![k[carryFood],v[7]]!
      | IN: ![k[carryFood],v[6]]!
      | IN: ![k[carryFood],v[3]]!
      | IN: ![k[carryFood],v[9]]!
      | IN: ![k[carryFood],v[4]]!
      | IN: ![k[carryFood],v[10]]!
      | IN: ![k[carryFood],v[8]]!
      | IN: ![k[getOnSnail: ],v[960,11,7]]!
      | IN: ![k[playerKill],v[751,462,2,1,Queen]]!
      | IN: ![k[blessMaiden],v[960,500,Blue]]!
      | IN: ![k[blessMaiden],v[1360,260,Blue]]!
      | IN: ![k[carryFood],v[3]]!
      | IN: ![k[carryFood],v[5]]!
      | IN: ![k[carryFood],v[10]]!
      | IN: ![k[carryFood],v[8]]!
      | IN: ![k[getOffSnail: ],v[1135,11,,7]]!
      | IN: ![k[blessMaiden],v[960,500,Red]]!
      | IN: ![k[blessMaiden],v[560,260,Red]]!
      | IN: ![k[getOnSnail: ],v[1135,11,7]]!
      | IN: ![k[getOffSnail: ],v[1147,11,,7]]!
      | IN: ![k[carryFood],v[3]]!
      | IN: ![k[blessMaiden],v[1510,860,Blue]]!""".stripMargin

  private[this] val bonusGameUnknownStart =
    """ IN: ![k[spawn],v[1,False]]!
      | IN: ![k[spawn],v[2,False]]!
      | IN: ![k[playernames],v[,,,,,,,,,]]!
      | IN: ![k[spawn],v[5,False]]!
      | IN: ![k[spawn],v[6,False]]!
      | IN: ![k[spawn],v[9,False]]!
      | IN: ![k[spawn],v[3,False]]!
      | IN: ![k[spawn],v[4,False]]!
      | IN: ![k[spawn],v[7,False]]!
      | IN: ![k[spawn],v[8,False]]!
      | IN: ![k[spawn],v[10,False]]!
      | IN: ![k[gamestart],v[map_dusk,False,0,False]]!""".stripMargin

  private[this] val regularGameStart =
    """ IN: ![k[spawn],v[1,False]]!
      | IN: ![k[spawn],v[2,False]]!
      | IN: ![k[playernames],v[,,,,,,,,,]]!
      | IN: ![k[spawn],v[5,False]]!
      | IN: ![k[spawn],v[6,False]]!
      | IN: ![k[spawn],v[9,False]]!
      | IN: ![k[spawn],v[3,True]]!
      | IN: ![k[spawn],v[4,True]]!
      | IN: ![k[spawn],v[7,True]]!
      | IN: ![k[spawn],v[8,True]]!
      | IN: ![k[spawn],v[10,True]]!
      | IN: ![k[gamestart],v[map_dusk,False,0,False]]!""".stripMargin

  private[this] def parseAndRunEvents(events: String, useHandler: Boolean = false): Unit = {
    val splitEvents = events.trim().split("\n").map(_.trim.stripPrefix("IN: ")).filter(_.nonEmpty)

    if (useHandler) {
      val parsedEvents = splitEvents.map(EventParser.parse)
      parsedEvents.zipWithIndex.foreach {
        case (event, idx) =>
          if (idx % 1000 == 0) {
            warn(s"Processing line $idx")
          }

          handler.handle(event)
      }
    } else {

      val parsedEvents = splitEvents.map(EventParser.parse).filter {
        case _: GameplayEvent => true
        case event => throw new Exception(s"Encountered unknown event $event")
      }.map(_.asInstanceOf[GameplayEvent])
      parsedEvents.foreach(sut.processEvent)
    }
  }


  private[this] implicit def findInPlayerMap(player: Player): PlayerState = sut.gameState.playerMap(player.id)

  private[this] def checkInvariants(): Unit = {
    // Only check invariants if the game ever started:
    if (sut.gameState.playerMap.contains(1)) {
      ensureNoIllegalDeposits()
      ensureSnailSanity()
    }
  }

  private[this] def ensureSnailSanity(): Unit = {
    // Queens can't move snails
    Player(1).distanceTraveledOnSnail should be(0)
    Player(2).distanceTraveledOnSnail should be(0)

    // NOTE: We used to have a "all players distance > 0" here, but it turns out you can move the snail back in some
    // cases, so that was an invalid check.  (For example, old builds let you bump snail backwards.)

    // If win by snail, teams total should be exactly the same as the expected distance
    // TODO: This won't be right on snail bonus game, so ignoring that case for now
    if (sut.gameState.winType.contains("snail")
      && sut.gameState.gameType == GameType.RegularGame) {
      val (playersOnWinningTeam, playersOnLosingTeam) = sut.gameState.playerList.partition(_.team == sut.gameState.victor.get)
      val winningTeamDistance = playersOnWinningTeam.map(_.distanceTraveledOnSnail).sum
      val losingTeamDistance = playersOnLosingTeam.map(_.distanceTraveledOnSnail).sum

      val expectedMapDistance = XYConstants.ScreenWidth / 2 - XYConstants.LeftGoalX(sut.gameState.map.get)
      (winningTeamDistance - losingTeamDistance) should be(expectedMapDistance)
    }
  }

  private[this] def ensureNoIllegalDeposits(): Unit = {
    // Queens can't deposit berries
    Player(1).foodDeposited should be(0)
    Player(2).foodDeposited should be(0)

    // Can't score food in a bonus game
    if (sut.gameState.gameType == GameType.MilitaryBonusGame || sut.gameState.gameType == GameType.SnailBonusGame) {
      sut.gameState.playerList.foreach {
        player =>
          player.foodDeposited should be(0)
      }
    }
  }

  private[this] var sut: StateMachine = _
  private[this] var handler: EventHandler = _

  override protected def beforeEach(): Unit = {
    super.beforeEach()

    sut = new StateMachine(exitOnTest = true)
    handler = new EventHandler(NoopEventSender, sut, checkInvariants)
  }

  override protected def afterEach(): Unit = {
    sut.exceptionFound should be(false)
  }

}
