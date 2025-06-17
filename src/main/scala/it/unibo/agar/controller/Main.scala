package it.unibo.agar.controller

import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.ddata.{ORSet, SelfUniqueAddress}
import akka.cluster.ddata.typed.scaladsl.DistributedData
import com.typesafe.config.ConfigFactory
import it.unibo.agar.model.{AIMovement, AkkaWorld, Food, GameInitializer, MockGameStateManager, Player}
import it.unibo.agar.view.{GlobalView, LocalView}

import java.awt.Window
import scala.swing.*
import scala.swing.Swing.onEDT
import scala.concurrent.duration.DurationInt

object Main extends SimpleSwingApplication:

  private val width = 1000
  private val height = 1000
  private val numPlayers = 4
  private val numFoods = 100
  private val players = GameInitializer.initialPlayers(numPlayers, width, height)
  private val foods = GameInitializer.initialFoods(numFoods, width, height)

  private case class Tick()

  private def behaviour(): Behavior[Tick] =
    Behaviors.setup(context =>
      given node: SelfUniqueAddress = DistributedData(context.system).selfUniqueAddress
      val ddPlayers = players.foldLeft(ORSet.empty[Player])((set, player) => set :+ player)
      val ddFoods = foods.foldLeft(ORSet.empty[Food])((set, food) => set :+ food)
      val manager = new MockGameStateManager(AkkaWorld(width, height, ddPlayers, ddFoods, node))
      // Open both views at startup
      new GlobalView(manager).open()
      new LocalView(manager, "p1").open()
      new LocalView(manager, "p2").open()
      Behaviors.withTimers(timers =>
        timers.startTimerAtFixedRate(Tick(), 30.millis)
        Behaviors.receiveMessage { case Tick() =>
          AIMovement.moveAI("p1", manager)
          manager.tick()
          onEDT(Window.getWindows.foreach(_.repaint()))
          Behaviors.same
        }
      )
    )

  override def top: Frame =
    val config = ConfigFactory
      .parseString(s"""akka.remote.artery.canonical.port=2551""")
      .withFallback(ConfigFactory.load("agario"))
    val system = ActorSystem(behaviour(), "AgarSystem", config)
    // No launcher window, just return an empty frame (or null if allowed)
    new Frame { visible = false }