package it.unibo.agar.view

import it.unibo.agar.model.{AkkaWorld, GameStateManager}

import java.awt.Graphics2D
import scala.swing.*
import scala.swing.Swing.onEDT

class LocalView(manager: GameStateManager, playerId: String, getWorld: () => AkkaWorld) extends MainFrame:

  title = s"Agar.io - Local View ($playerId)"
  preferredSize = new Dimension(400, 400)

  contents = new Panel:
    listenTo(keys, mouse.moves)
    focusable = true
    requestFocusInWindow()

    // Paint using a snapshot provided by Main via getWorld()
    override def paintComponent(g: Graphics2D): Unit =
      val world = getWorld()
      val playerOpt = world.players.find(_.id == playerId)
      val (offsetX, offsetY) = playerOpt
        .map(p => (p.x - size.width / 2.0, p.y - size.height / 2.0))
        .getOrElse((0.0, 0.0))
      AgarViewUtils.drawWorld(g, world, offsetX, offsetY)

    // Mouse movement drives local input; we still forward input to manager
    reactions += { case e: event.MouseMoved =>
      val mousePos = e.point
      // Use snapshot for centering calculation too (consistent)
      val world = getWorld()
      val playerOpt = world.players.find(_.id == playerId)
      playerOpt.foreach: player =>
        val dx = (mousePos.x - size.width / 2) * 0.01
        val dy = (mousePos.y - size.height / 2) * 0.01
        // Forward input to manager (manager handles applying input to local player)
        manager.movePlayerDirection(playerId, dx, dy)
      repaint()
    }

  def render(): Unit = onEDT(repaint())
