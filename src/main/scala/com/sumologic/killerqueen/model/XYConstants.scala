package com.sumologic.killerqueen.model

object XYConstants {

  // This is configurable in console settings and not accurate at the start for all cabinets.  The StateMachine will
  // actually fix it if it detects an illegal BerryDeposit.
  var LeftTeam = "Blue"
  var RightTeam = "Gold"

  val ScreenWidth = 1920
  val ScreenHeight = 1080


  def teamSideFromXCoordinate(x: Int): String = {
    if (x < ScreenWidth / 2) {
      LeftTeam
    } else {
      RightTeam
    }
  }


  // FIXME: Does not include snail bonus game.  And that'll break the API since there are 3 goals instead of 1
  val LeftGoalX: Map[String, Int] = Map(
    // FIXME: Neither goal is right on the edge, so these are approximate.  I'll need to refine these.
    "map_day" -> 20,
    "map_dusk" -> 20,
    "map_night" -> 100
  )

  val RightGoalX: Map[String, Int] = LeftGoalX.mapValues(ScreenWidth - _)

  val GoalY: Map[String, Int] = Map(
    "map_day" -> 11,
    "map_dusk" -> 11,
    "map_night" -> 491
  )

  def snailGoalCoordinatesForTeam(team: String, map: String): (Int, Int) = {
    if (team == LeftTeam) {
      (LeftGoalX(map), GoalY(map))
    } else {
      (RightGoalX(map), GoalY(map))
    }
  }

  // TODO: Fix left vs right spawn instead of gold vs blue
  val BonusGameMap = "map_bonus"
  val Regions: Map[String, Seq[XYRegion]] = Map(
    BonusGameMap -> Seq.empty,

    "map_day" -> Seq(
      XYRegion((720, 860), (940, 1080), "gold_spawn"),
      XYRegion((980, 860), (1200, 1080), "blue_spawn")
    ),

    "map_dusk" -> Seq(
      XYRegion((680, 520), (920, 840), "gold_spawn"),
      XYRegion((1000, 520), (1240, 840), "blue_spawn")
    ),

    "map_night" -> Seq(
      XYRegion((20, 20), (360, 240), "gold_spawn"),
      XYRegion((1560, 20), (1900, 240), "blue_spawn")
    )
  )

  def locationForXYOnMap(x: Int, y: Int, map: String): String = {
    val regionList = Regions.getOrElse(map, Seq.empty)
    regionList.find {
      region =>
        x >= region.bottomLeft._1 &&
          y >= region.bottomLeft._2 &&
          x <= region.topRight._1 &&
          y <= region.topRight._2
    }.map(_.name).getOrElse("unknown")
  }

  case class XYRegion(bottomLeft: (Int, Int), topRight: (Int, Int), name: String) {
    require(bottomLeft._1 < topRight._1)
    require(bottomLeft._2 < topRight._2)
  }

}
