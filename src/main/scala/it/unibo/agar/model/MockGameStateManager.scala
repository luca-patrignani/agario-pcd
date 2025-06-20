package it.unibo.agar.model

trait GameStateManager:

  def getWorld: World
  def movePlayerDirection(id: String, dx: Double, dy: Double): Unit

class MockGameStateManager(
                            var world: AkkaWorld,
                            val speed: Double = 10.0
) extends GameStateManager:

  private var directions: Map[String, (Double, Double)] = Map.empty
  def getWorld: AkkaWorld = world

  // Move a player in a given direction (dx, dy)
  def movePlayerDirection(id: String, dx: Double, dy: Double): Unit =
    directions = directions.updated(id, (dx, dy))

  def tick(world: AkkaWorld): Unit = {
    this.world = world
    directions.foreach:
      case (id, (dx, dy)) =>
        this.world.playerById(id) match
          case Some(player) =>
            this.world = updateWorldAfterMovement(updatePlayerPosition(player, dx, dy))
          case None =>
  }
  // Player not found, ignore movement

  private def updatePlayerPosition(player: Player, dx: Double, dy: Double): Player =
    val newX = (player.x + dx * speed).max(0).min(world.width)
    val newY = (player.y + dy * speed).max(0).min(world.height)
    player.copy(x = newX, y = newY)

  private def updateWorldAfterMovement(player: Player): AkkaWorld =
    val foodEaten = world.foods.filter(food => EatingManager.canEatFood(player, food))
    val playerEatsFood = foodEaten.foldLeft(player)((p, food) => p.grow(food))
    val playersEaten = world
      .playersExcludingSelf(player)
      .filter(player => EatingManager.canEatPlayer(playerEatsFood, player))
    val playerEatPlayers = playersEaten.foldLeft(playerEatsFood)((p, other) => p.grow(other))
    world
      .updatePlayer(playerEatPlayers)
      .removePlayers(playersEaten.map(_.id))
      .removeFoods(foodEaten.map(_.id))
