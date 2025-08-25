package it.unibo.agar.model

import scala.util.Random.nextInt

case class AkkaWorld(
    width: Int,
    height: Int,
    _players: Map[String, Player] = Map.empty,
    _foods: Map[String, Food] = Map.empty,
    removedEntitiesIds: Seq[String] = Seq.empty
) extends World:

  def updatePlayer(player: Player): AkkaWorld =
    copy(_players = _players.updated(player.id, player))

  def removePlayers(ids: Seq[String]): AkkaWorld =
    copy(_players = _players.removedAll(ids), removedEntitiesIds = removedEntitiesIds ++ ids)

  def addFoods(quantity: Int): AkkaWorld =
    val newFoods = (1 to quantity).map(i => Food(s"f${nextInt()}", nextInt(width), nextInt(height)))
    copy(_foods = _foods ++ newFoods.map(food => food.id -> food))

  def removeFoods(ids: Seq[String]): AkkaWorld =
    copy(_foods = _foods.removedAll(ids), removedEntitiesIds = removedEntitiesIds ++ ids)

  override def players: Seq[Player] = _players.filterNot((id, player) => removedEntitiesIds.contains(id)).values.toSeq

  override def foods: Seq[Food] = _foods.filterNot((id, food) => removedEntitiesIds.contains(id)).values.toSeq
