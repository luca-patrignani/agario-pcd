package it.unibo.agar.model

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.Behavior
import akka.cluster.ddata.typed.scaladsl.DistributedData
import akka.cluster.ddata.typed.scaladsl.Replicator
import akka.cluster.ddata.typed.scaladsl.Replicator.Update
import akka.cluster.ddata.LWWMap
import akka.cluster.ddata.LWWMapKey
import akka.cluster.ddata.SelfUniqueAddress
import it.unibo.agar.model.*

import scala.concurrent.duration.*

object FoodManager:

  sealed trait Command
  case object SpawnInitial extends Command
  case object RespawnTick extends Command

  private val RespawnInterval = 5.seconds
  private val NumFoods = 100

  def apply(width: Int, height: Int): Behavior[Command] =
    Behaviors.setup { ctx =>
      DistributedData.withReplicatorMessageAdapter[Command, LWWMap[String, Entity]] { replicator =>
        given SelfUniqueAddress = DistributedData(ctx.system).selfUniqueAddress
        val EntitiesKey = LWWMapKey[String, Entity]("entities")

        ctx.log.info("FoodManager singleton started on {}", EntitiesKey)

        val foods = GameInitializer.initialFoods(NumFoods, width, height)

        def updateRemoteFood(foods: Seq[Food]): Unit =
          replicator.askUpdate(
            replyTo =>
              Update(EntitiesKey, LWWMap.empty[String, Entity], Replicator.WriteLocal, replyTo) { map =>
                foods.foldLeft(map)((m, f) => m.put(summon, f.id, f))
              },
            _ => SpawnInitial
          )

        updateRemoteFood(foods)

        Behaviors.withTimers { timers =>
          timers.startTimerWithFixedDelay(RespawnTick, RespawnInterval)

          Behaviors.receiveMessage {
            case RespawnTick =>
              val newFoods = (1 to 5).map[Food](_ => Food.random(width, height))
              updateRemoteFood(newFoods)
              Behaviors.same
            case _ => Behaviors.same
          }
        }
      }
    }
