package com.sumologic.killerqueen.model

import com.sumologic.killerqueen.TestBase

class PlayerTest extends TestBase {
  "Player.apply" should {
    "correctly pick the names" in {
      Player(1) should be(Player(1, "Gold Queen"))
      Player(2) should be(Player(2, "Blue Queen"))
      Player(3) should be(Player(3, "Gold Stripes"))
      Player(4) should be(Player(4, "Blue Stripes"))
      Player(5) should be(Player(5, "Gold Abs"))
      Player(6) should be(Player(6, "Blue Abs"))
      Player(7) should be(Player(7, "Gold Skulls"))
      Player(8) should be(Player(8, "Blue Skulls"))
      Player(9) should be(Player(9, "Gold Checks"))
      Player(10) should be(Player(10, "Blue Checks"))

      Player(1).isQueen should be(true)
      Player(2).isQueen should be(true)
      (3 to 10).foreach(Player.apply(_).isQueen should be(false))
    }
  }
}
