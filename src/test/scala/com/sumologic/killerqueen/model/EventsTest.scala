package com.sumologic.killerqueen.model

import com.sumologic.killerqueen.TestBase

class EventsTest extends TestBase {

  "InboundEvents.BlessMaidenEvent" should {
    "properly translate between Gold and Red" in {
      InboundEvents.BlessMaidenEvent(1, 2, "Gold").toApi should be("![k[blessMaiden],v[1,2,Red]]!")
    }
  }

}
