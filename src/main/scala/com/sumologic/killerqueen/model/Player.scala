package com.sumologic.killerqueen.model

case class Player(id: Int, name: String) {
  val team = Player.colors((id - 1) % 2)

  def isQueen: Boolean = id <= 2
}

object Player {
  private[this] val names = Seq("Queen", "Stripes", "Abs", "Skulls", "Checks")
  private val colors = Seq("Gold", "Blue")

  def apply(id: Int): Player = {
    val zeroedId = id - 1
    val color = colors(zeroedId % 2)
    val playerName = s"$color ${names(zeroedId / 2)}"
    Player(id, playerName)
  }
}
