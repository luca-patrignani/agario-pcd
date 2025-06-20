package it.unibo.agar.model

trait World:
  val width: Int
  val height: Int
  def players: Seq[Player]
  def foods: Seq[Food]

  def playersExcludingSelf(player: Player): Seq[Player] =
    players.filterNot(_.id == player.id)

  def playerById(id: String): Option[Player] =
    players.find(_.id == id)

