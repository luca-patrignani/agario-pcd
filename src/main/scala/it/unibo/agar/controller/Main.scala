package it.unibo.agar.controller

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior}
import akka.cluster.ddata
import akka.cluster.ddata.typed.scaladsl.Replicator.{Update, WriteLocal}
import akka.cluster.ddata.typed.scaladsl.{DistributedData, Replicator}
import akka.cluster.ddata.*
import com.typesafe.config.ConfigFactory
import it.unibo.agar.model.*
import it.unibo.agar.view.LocalView

import java.awt.Window
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.swing.*
import scala.swing.Swing.onEDT

case class Main(id: String) extends SimpleSwingApplication:

  private val width = 1000
  private val height = 1000
  private val numFoods = 100
  private val players = GameInitializer.initialPlayers(2, width, height)
  private val foods = GameInitializer.initialFoods(numFoods, width, height)

  trait Command
  case class Tick() extends Command
  case class EntitiesChanged(changed: Replicator.SubscribeResponse[LWWMap[String, Entity]]) extends Command
  case class EntitiesRemoved(removed: Replicator.SubscribeResponse[GSet[String]]) extends Command
  case class Show() extends Command
  case class Ignore() extends Command

  private def behaviour(): Behavior[Command] =
    Behaviors.setup(context =>
      DistributedData.withReplicatorMessageAdapter[Command, LWWMap[String, Entity]] { entitiesReplicator =>
        DistributedData.withReplicatorMessageAdapter[Command, GSet[String]] { removedReplicator =>
          given node: SelfUniqueAddress = DistributedData(context.system).selfUniqueAddress

          val EntitiesKey = LWWMapKey[String, Entity]("entities")
          val RemovedKey = GSetKey[String]("removedEntities")
          entitiesReplicator.subscribe(EntitiesKey, EntitiesChanged.apply)
          removedReplicator.subscribe(RemovedKey, EntitiesRemoved.apply)


          def updateRemoteWorld(world: AkkaWorld): Unit = {
            entitiesReplicator.askUpdate(
              askReplyTo => Update(EntitiesKey, LWWMap.empty[String, Entity], WriteLocal, askReplyTo)
                (map => world.playerById(id).map(player => map.put(node, id, player)).getOrElse(map)),
              {
                case Replicator.UpdateSuccess(_) => Show()
                case _ => Show()
              }
            )
            removedReplicator.askUpdate(
              askReplyTo => Update(RemovedKey, GSet.empty[String], WriteLocal, askReplyTo)
                (world.removedEntitiesIds.foldLeft(_)((set, id) => set.add(id))),
            _ => Ignore())
          }

          var world = AkkaWorld(width, height)
            .updatePlayer(Player(id, width / 2.0, height / 2.0, 120.0))
            .addFoods(100)

          entitiesReplicator.askUpdate(
            askReplyTo => Update(EntitiesKey, LWWMap.empty[String, Entity], WriteLocal, askReplyTo)
              ((world.players ++ world.foods).foldLeft(_)((map, entity) => map.put(node, entity.id, entity))),
            _ => Ignore()
          )
          val manager = new MockGameStateManager(world)
          // Open both views at startup
          // new GlobalView(manager).open()
          new LocalView(manager, id).open()
          Behaviors.withTimers(timers =>
            timers.startTimerAtFixedRate(Tick(), 30.millis)
            Behaviors.receiveMessage {
              case Tick() =>
                manager.tick(world)
                updateRemoteWorld(manager.getWorld)
                world = manager.getWorld
                Behaviors.same
              case Show() =>
                onEDT(Window.getWindows.foreach(_.repaint()))
                Behaviors.same
              case EntitiesRemoved(change @ Replicator.Changed(key)) => key match
                case key if key == RemovedKey =>
                  val removedEntities = change.get(RemovedKey).elements
                  world = world.removePlayers(removedEntities.toSeq)
                  world = world.removeFoods(removedEntities.toSeq)
                case _ =>
                  val entities = change.get(EntitiesKey).entries.values
                  entities.foreach {
                    case player: Player =>
                      if (player.id != id) {
                        world = world.updatePlayer(player)
                      }
                    case food: Food =>
                      world = world.copy(_foods = world._foods.updated(food.id, food))
                  }
                Behaviors.same
              case Ignore() => Behaviors.same
            }
          )
        }
      }
    )

  def startPlayer(port: String): ActorSystem[Command] =
    val config = ConfigFactory
      .parseString(s"""akka.remote.artery.canonical.port=$port""")
      .withFallback(ConfigFactory.load("agario"))
    ActorSystem(behaviour(), "AgarSystem", config)

  override def top: Frame = {
    startPlayer(id)
    new Frame { visible = false }
  }

@main
def firstPlayer() =
  Main("2551").top

@main
def secondPlayer() =
  Main("2552").top
