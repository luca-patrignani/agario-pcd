package it.unibo.agar.controller

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.Behavior
import akka.cluster.ddata.typed.scaladsl.{DistributedData, Replicator}
import akka.cluster.ddata.typed.scaladsl.Replicator.{Update, UpdateFailure, UpdateSuccess, WriteLocal}
import akka.cluster.ddata.{LWWMap, LWWMapKey, SelfUniqueAddress}
import it.unibo.agar.model.*
import it.unibo.agar.view.{GlobalView, LocalView}

import java.awt.Window
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*
import scala.swing.Swing.onEDT

object PlayerNode:
  sealed trait Command
  case object Tick extends Command
  case object GcTick extends Command
  final case class EntitiesChanged(resp: Replicator.SubscribeResponse[LWWMap[String, Entity]]) extends Command
  case object Show extends Command
  case object Ignore extends Command

  private val width  = 1000
  private val height = 1000
  private val positionThreshold = 2.0
  private val networkInterval   = 200.millis
  private val gcInterval        = 60.seconds
  private val renderingInterval = 30.millis
  private val tombstoneTtl      = 5.minutes

  def apply(id: String): Behavior[Command] =
    Behaviors.setup { context =>
      DistributedData.withReplicatorMessageAdapter[Command, LWWMap[String, Entity]] { replicator =>
        given SelfUniqueAddress = DistributedData(context.system).selfUniqueAddress
        val EntitiesKey = LWWMapKey[String, Entity]("entities")

        replicator.subscribe(EntitiesKey, rsp => EntitiesChanged(rsp))

        val initialWorld = AkkaWorld(width, height).addPlayers(Player(id, width/2.0, height/2.0, 120.0))
        val manager = new MockGameStateManager(initialWorld)
        val uiWorldRef = new AtomicReference[AkkaWorld](initialWorld)

        new GlobalView(manager).open()
        new LocalView(manager, id, () => uiWorldRef.get()).open()

        def writeMyState(p: Player): Unit =
          replicator.askUpdate(
            replyTo => Update(EntitiesKey, LWWMap.empty[String, Entity], WriteLocal, replyTo)(_.put(summon, p.id, p)),
            {
              case UpdateSuccess(_) => Show
              case UpdateFailure(_) => Ignore
              case _                => Ignore
            }
          )

        val localPlayer = initialWorld.playerById(id).get
        writeMyState(localPlayer)

        def integrateEntities(local: AkkaWorld, states: Iterable[Entity]): AkkaWorld =
          states.foldLeft(local) { (w, s) =>
            if (!s.alive && (System.currentTimeMillis() - s.lastSeen) > tombstoneTtl.toMillis)
              w.removePlayers(Seq(s.id)).removeFoods(Seq(s.id))
            else
              s match
                case p: Player => w.updatePlayer(p)
                case f: Food   => w.copy(_foods = w._foods.updated(f.id, f))
                case _ => w
          }

        Behaviors.withTimers { timers =>
          timers.startTimerWithFixedDelay(Tick, renderingInterval)
          timers.startTimerWithFixedDelay(GcTick, gcInterval)

          def loop(world: AkkaWorld, lastSentPos: Option[(Double, Double)], lastNetworkUpdate: Long): Behavior[Command] =
            Behaviors.receiveMessage {
              case Tick =>
                manager.tick(world)
                val updatedWorld = manager.getWorld
                uiWorldRef.set(updatedWorld)
                onEDT(Window.getWindows.foreach(_.repaint()))

                val myPlayerOpt = updatedWorld.playerById(id)
                val now = System.currentTimeMillis()
                val sendNow = myPlayerOpt.exists { p =>
                  lastSentPos.forall { case (sx, sy) => p.distanceTo(sx, sy) > positionThreshold } ||
                    (now - lastNetworkUpdate) >= networkInterval.toMillis
                }
                if sendNow then myPlayerOpt.foreach(writeMyState)
                manager.setWorld(updatedWorld)
                loop(updatedWorld, myPlayerOpt.map(p => (p.x, p.y)).orElse(lastSentPos), if sendNow then now else lastNetworkUpdate)

              case EntitiesChanged(change @ Replicator.Changed(EntitiesKey)) =>
                val map = change.get(EntitiesKey)
                val merged = integrateEntities(world, map.entries.values)
                manager.setWorld(merged)
                uiWorldRef.set(merged)
                onEDT(Window.getWindows.foreach(_.repaint()))
                loop(merged, lastSentPos, lastNetworkUpdate)

              case GcTick =>
                replicator.askUpdate(
                  replyTo => Update(EntitiesKey, LWWMap.empty[String, Entity], WriteLocal, replyTo) { map =>
                    val now = System.currentTimeMillis()
                    map.entries.foldLeft(map) { case (m, (k, s)) =>
                      if (!s.alive && (now - s.lastSeen) > tombstoneTtl.toMillis) m.remove(summon, k)
                      else m
                    }
                  },
                  _ => Ignore
                )
                Behaviors.same

              case Show =>
                onEDT(Window.getWindows.foreach(_.repaint()))
                Behaviors.same

              case Ignore => Behaviors.same
            }

          loop(manager.getWorld, None, 0L)
        }
      }
    }
