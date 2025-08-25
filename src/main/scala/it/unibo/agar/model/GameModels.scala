package it.unibo.agar.model

import scala.util.Random

sealed trait Entity:

  def id: String
  def mass: Double
  def x: Double
  def y: Double
  def radius: Double = math.sqrt(mass / math.Pi)

  def distanceTo(other: Entity): Double =
    val dx = x - other.x
    val dy = y - other.y
    math.hypot(dx, dy)

case class Player(id: String, x: Double, y: Double, mass: Double) extends Entity:

  def grow(entity: Entity): Player =
    copy(mass = mass + entity.mass)

case class Food(id: String, x: Double, y: Double, mass: Double = 100.0) extends Entity
object Food:

  def random(w: Int, h: Int): Food =
    Food(s"f${Random.nextInt()}", Random.nextInt(w), Random.nextInt(h))

case class LocalWorld(
    width: Int,
    height: Int,
    players: Seq[Player],
    foods: Seq[Food]
) extends World:

  def updatePlayer(player: Player): LocalWorld =
    copy(players = players.map(p => if (p.id == player.id) player else p))

  def removePlayers(ids: Seq[Player]): LocalWorld =
    copy(players = players.filterNot(p => ids.map(_.id).contains(p.id)))

  def removeFoods(ids: Seq[Food]): LocalWorld =
    copy(foods = foods.filterNot(f => ids.contains(f)))
