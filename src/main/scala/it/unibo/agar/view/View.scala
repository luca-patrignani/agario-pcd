package it.unibo.agar.view

import it.unibo.agar.model.GameStateManager
import java.awt.{Graphics2D, Dimension}
import scala.swing.*
import scala.swing.Swing.onEDT

abstract class View(manager: GameStateManager) extends MainFrame:
  def render(): Unit = onEDT(repaint())

class LocalView(manager: GameStateManager, playerId: String) extends View(manager):

  title = s"Agar.io - Local View (${playerId.drop(3)})"
  preferredSize = new Dimension(400, 400)

  contents = new Panel { self =>
    listenTo(keys, mouse.moves)
    focusable = true
    requestFocusInWindow()

    override def paintComponent(g: Graphics2D): Unit =
      super.paintComponent(g)

      val world = manager.getWorld
      val playerOpt = world.players.find(_.id == playerId)

      val (offsetX, offsetY) = playerOpt
        .map(p => (p.x - size.width / 2.0, p.y - size.height / 2.0))
        .getOrElse((0.0, 0.0))

      AgarViewUtils.drawWorld(g, world, offsetX, offsetY)

    reactions += {
      case e: event.MouseMoved =>
        val mousePos = e.point
        manager.getWorld.players.find(_.id == playerId).foreach { player =>
          val dx = (mousePos.x - size.width / 2) * 0.01
          val dy = (mousePos.y - size.height / 2) * 0.01
          manager.movePlayerDirection(playerId, dx, dy)
        }
        self.repaint()
    }
  }

class GlobalView(manager: GameStateManager) extends View(manager):

  title = "Agar.io - Global View"
  preferredSize = new Dimension(800, 800)

  contents = new Panel:
    override def paintComponent(g: Graphics2D): Unit =
      super.paintComponent(g)
      val world = manager.getWorld
      AgarViewUtils.drawWorld(g, world)
