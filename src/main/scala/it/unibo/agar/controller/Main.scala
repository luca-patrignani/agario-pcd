package it.unibo.agar.controller

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.cluster.ddata.{LWWMap, LWWMapKey, LWWRegister, LWWRegisterKey, ORSet, PNCounterMapKey, SelfUniqueAddress}
import akka.cluster.ddata.typed.scaladsl.{DistributedData, Replicator}
import akka.cluster.ddata.typed.scaladsl.Replicator.{Get, Update, WriteLocal}
import akka.cluster.typed.Cluster
import com.typesafe.config.ConfigFactory
import it.unibo.agar.model.{AIMovement, AkkaWorld, Entity, Food, GameInitializer, LocalWorld, MockGameStateManager, Player, World}
import it.unibo.agar.view.{GlobalView, LocalView}

import java.awt.Window
import scala.swing.*
import scala.swing.Swing.onEDT
import scala.concurrent.duration.DurationInt

object Main extends SimpleSwingApplication:

  private val width = 1000
  private val height = 1000
  private val numFoods = 100
  private val players = GameInitializer.initialPlayers(2, width, height)
  private val foods = GameInitializer.initialFoods(numFoods, width, height)

  trait Command
  case class Tick() extends Command
  case class SubscribeResponse(changed: Replicator.SubscribeResponse[LWWMap[String, Entity]]) extends Command
  case class Show() extends Command

  private def behaviour(id: String): Behavior[Command] =
    Behaviors.setup(context =>
      DistributedData.withReplicatorMessageAdapter[Command, LWWMap[String, Entity]] { replicator =>
        given node: SelfUniqueAddress = DistributedData(context.system).selfUniqueAddress

        val entitiesKey = LWWMapKey[String, Entity]("entities")
        replicator.subscribe(entitiesKey, SubscribeResponse.apply)

        def updateRemoteWorld(world: World): Behavior[Command] =
          replicator.askUpdate(
            askReplyTo => Update(entitiesKey, LWWMap.empty[String, Entity], WriteLocal, askReplyTo)
              ((world.players concat world.foods).foldLeft(_)((map, entity) => map.put(node, entity.id, entity))),
            {
              case Replicator.UpdateSuccess(_) => Show()
              case _ => ???
            }
          )
          Behaviors.same

        val manager = new MockGameStateManager(LocalWorld(width, height, players, foods))
        // Open both views at startup
        // new GlobalView(manager).open()
        new LocalView(manager, id).open()
        var world = manager.getWorld
        Behaviors.withTimers(timers =>
          timers.startTimerAtFixedRate(Tick(), 30.millis)
          Behaviors.receiveMessage {
            case Tick() =>
              manager.tick(world)
              updateRemoteWorld(manager.getWorld)
            case Show() =>
              world = manager.getWorld
              onEDT(Window.getWindows.foreach(_.repaint()))
              Behaviors.same
            case SubscribeResponse(changed @ Replicator.Changed(entitiesKey)) =>
              val entities = changed.get(entitiesKey).entries.values
              world = LocalWorld(width, height,
                entities.collect { case p: Player => p }.toSeq,
                entities.collect { case f: Food => f }.toSeq
              )
              Behaviors.same
          }
        )
      }
    )

  def startPlayer(port: Int): ActorSystem[Command] =
    val config = ConfigFactory
      .parseString(s"""akka.remote.artery.canonical.port=$port""")
      .withFallback(ConfigFactory.load("agario"))
    ActorSystem(behaviour(s"p${port % 10}"), "AgarSystem", config)  

  override def top: Frame = {
    startPlayer(2551)
    startPlayer(2552)
    new Frame { visible = false }
  }