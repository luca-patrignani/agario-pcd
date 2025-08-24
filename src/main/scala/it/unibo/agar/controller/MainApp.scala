package it.unibo.agar.controller

import akka.actor.typed.ActorSystem
import akka.cluster.typed.{ClusterSingleton, SingletonActor}
import com.typesafe.config.ConfigFactory

object MainApp:
  def startPlayer(port: Int, id: String): ActorSystem[PlayerNode.Command] =
    val config = ConfigFactory
      .parseString(s"akka.remote.artery.canonical.port=$port")
      .withFallback(ConfigFactory.load("agario"))

    val system = ActorSystem(PlayerNode(id), "AgarSystem", config)

    // start cluster singleton FoodManager
    val singleton = ClusterSingleton(system)
    singleton.init(SingletonActor(FoodManager(), "FoodManager"))

    system

@main def firstPlayer(): Unit =
  MainApp.startPlayer(2551, "player1")

@main def secondPlayer(): Unit =
  MainApp.startPlayer(2552, "player2")
