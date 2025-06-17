package it.unibo.agar.model

import akka.cluster.ddata.{ORSet, ReplicatedData, SelfUniqueAddress}

case class AkkaWorld(width: Int, height: Int, _players: ORSet[Player], _foods: ORSet[Food], node: SelfUniqueAddress)
  extends World with ReplicatedData:

  given SelfUniqueAddress = node

  def players: Seq[Player] = _players.elements.toSeq

  def foods: Seq[Food] = _foods.elements.toSeq

  override def updatePlayer(player: Player): World =
    playerById(player.id) match
      case Some(removingPlayer) =>
        copy(
          _players = _players.remove(removingPlayer) :+ player
        )
      case _ => this

  override def removePlayers(ids: Seq[Player]): World =
    copy(
      _players = ids
        .map(_.id)
        .flatMap(playerById)
        .foldLeft(_players)((set, player) => set.remove(player))
    )

  override def removeFoods(ids: Seq[Food]): World =
    copy(
      _foods = ids.foldLeft(_foods)((set, food) => set.remove(food))
    )

  override type T = AkkaWorld

  override def merge(that: AkkaWorld): AkkaWorld =
    AkkaWorld(
      width,
      height,
      _players.merge(that._players),
      _foods.merge(that._foods),
      node
    )
