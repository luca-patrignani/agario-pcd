package it.unibo.agar.controller

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.Behavior
import akka.cluster.ddata.typed.scaladsl.{DistributedData, Replicator}
import akka.cluster.ddata.typed.scaladsl.Replicator.Update
import akka.cluster.ddata.{LWWMap, LWWMapKey, SelfUniqueAddress}
import it.unibo.agar.model.*

import scala.concurrent.duration.*

object FoodManager:
  sealed trait Command
  case object SpawnInitial extends Command
  case object RespawnTick extends Command

  private val RespawnInterval = 10.seconds
  private val NumFoods = 100
  private val Width  = 1000
  private val Height = 1000

  def apply(): Behavior[Command] =
    Behaviors.setup { ctx =>
      DistributedData.withReplicatorMessageAdapter[Command, LWWMap[String, Entity]] { replicator =>
        given SelfUniqueAddress = DistributedData(ctx.system).selfUniqueAddress
        val EntitiesKey = LWWMapKey[String, Entity]("entities")

        ctx.log.info("FoodManager singleton started on {}", EntitiesKey)

        // initial spawn
        val foods = GameInitializer.initialFoods(NumFoods, Width, Height)
        replicator.askUpdate(
          replyTo => Update(EntitiesKey, LWWMap.empty[String, Entity], Replicator.WriteLocal, replyTo) { map =>
            foods.foldLeft(map)((m, f) => m.put(summon, f.id, f))
          },
          _ => SpawnInitial
        )

        Behaviors.withTimers { timers =>
          timers.startTimerWithFixedDelay(RespawnTick, RespawnInterval)

          Behaviors.receiveMessage {
            case RespawnTick =>
              val newFoods = GameInitializer.initialFoods(5, Width, Height)
              replicator.askUpdate(
                replyTo => Update(EntitiesKey, LWWMap.empty[String, Entity], Replicator.WriteLocal, replyTo) { map =>
                  newFoods.foldLeft(map)((m, f) => m.put(summon, f.id, f))
                },
                _ => SpawnInitial
              )
              Behaviors.same
            case _ => Behaviors.same
          }
        }
      }
    }
